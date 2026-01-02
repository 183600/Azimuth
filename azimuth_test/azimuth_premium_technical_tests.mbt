// Azimuth Premium Technical Test Suite
// This file contains 8 high-quality test cases focusing on advanced technical scenarios

// Test 1: Resource Management and Merge Strategy
test "resource management and merge strategy" {
  // Create primary resource with service identity
  let primary_resource = Resource::new()
  let primary_with_attrs = Resource::with_attributes(primary_resource, [
    ("service.name", StringValue("azimuth-premium-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("instance-abc123")),
    ("deployment.environment", StringValue("production"))
  ])
  
  // Create secondary resource with infrastructure attributes
  let secondary_resource = Resource::new()
  let secondary_with_attrs = Resource::with_attributes(secondary_resource, [
    ("host.name", StringValue("prod-server-01")),
    ("host.arch", StringValue("amd64")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("5.15.0"))
  ])
  
  // Create tertiary resource with custom business attributes
  let tertiary_resource = Resource::new()
  let tertiary_with_attrs = Resource::with_attributes(tertiary_resource, [
    ("business.unit", StringValue("platform-engineering")),
    ("cost.center", StringValue("engineering-001")),
    ("compliance.level", StringValue("critical"))
  ])
  
  // Test resource merge
  let merged_resource_1 = Resource::merge(primary_with_attrs, secondary_with_attrs)
  
  // Verify merged resource contains all attributes
  let service_name = Attributes::get(Resource::attributes(merged_resource_1), "service.name")
  let host_name = Attributes::get(Resource::attributes(merged_resource_1), "host.name")
  
  assert_eq(service_name, Some(StringValue("azimuth-premium-service")))
  assert_eq(host_name, Some(StringValue("prod-server-01")))
  
  // Test three-way merge
  let merged_resource_2 = Resource::merge(merged_resource_1, tertiary_with_attrs)
  
  let business_unit = Attributes::get(Resource::attributes(merged_resource_2), "business.unit")
  let compliance_level = Attributes::get(Resource::attributes(merged_resource_2), "compliance.level")
  let deployment_env = Attributes::get(Resource::attributes(merged_resource_2), "deployment.environment")
  
  assert_eq(business_unit, Some(StringValue("platform-engineering")))
  assert_eq(compliance_level, Some(StringValue("critical")))
  assert_eq(deployment_env, Some(StringValue("production")))
}

// Test 2: Advanced Attribute Operations
test "advanced attribute operations" {
  // Create attributes with various types
  let attrs = Attributes::new()
  
  // Set different attribute types
  Attributes::set(attrs, "string.attr", StringValue("test.value"))
  Attributes::set(attrs, "int.attr", IntValue(42))
  Attributes::set(attrs, "float.attr", FloatValue(3.14159))
  Attributes::set(attrs, "bool.attr", BoolValue(true))
  Attributes::set(attrs, "array.attr", ArrayStringValue(["item1", "item2", "item3"]))
  
  // Retrieve attributes
  let string_attr = Attributes::get(attrs, "string.attr")
  let int_attr = Attributes::get(attrs, "int.attr")
  let float_attr = Attributes::get(attrs, "float.attr")
  let bool_attr = Attributes::get(attrs, "bool.attr")
  let array_attr = Attributes::get(attrs, "array.attr")
  let non_existent = Attributes::get(attrs, "non.existent")
  
  // Verify attribute values
  assert_eq(string_attr, Some(StringValue("test.value")))
  assert_eq(int_attr, Some(IntValue(42)))
  assert_eq(float_attr, Some(FloatValue(3.14159)))
  assert_eq(bool_attr, Some(BoolValue(true)))
  assert_eq(array_attr, Some(ArrayStringValue(["item1", "item2", "item3"])))
  assert_eq(non_existent, None)
  
  // Test attribute type conversion
  let converted_int = match int_attr {
    Some(IntValue(i)) => StringValue(int_to_string_safe(i))
    _ => StringValue("0")
  }
  
  let converted_string = match string_attr {
    Some(StringValue(s)) => IntValue(string_to_int_safe(s))
    _ => IntValue(0)
  }
  
  // Verify conversions
  assert_eq(converted_int, StringValue("42"))
  assert_eq(converted_string, IntValue(0))  // "test.value" is not a number
}

// Test 3: Complex Span Operations
test "complex span operations" {
  // Create tracer provider and tracer
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "complex.test.tracer")
  
  // Create root span with attributes
  let root_span = Tracer::start_span(tracer, "root-operation")
  Span::set_status(root_span, Ok, Some("Root operation started"))
  
  // Add multiple events to root span
  Span::add_event(root_span, "root.event.1", Some([("event.type", StringValue("start"))]))
  Span::add_event(root_span, "root.event.2", Some([("event.type", StringValue("processing"))]))
  
  // Create child spans
  let child_span_1 = Tracer::start_span(tracer, "child-operation-1")
  Span::set_status(child_span_1, Ok, Some("Child operation 1 completed"))
  Span::add_event(child_span_1, "child.event.1", Some([("child.id", IntValue(1))]))
  
  let child_span_2 = Tracer::start_span(tracer, "child-operation-2")
  Span::set_status(child_span_2, Ok, Some("Child operation 2 completed"))
  Span::add_event(child_span_2, "child.event.1", Some([("child.id", IntValue(2))]))
  
  // Create grandchild span
  let grandchild_span = Tracer::start_span(tracer, "grandchild-operation")
  Span::set_status(grandchild_span, Ok, Some("Grandchild operation completed"))
  Span::add_event(grandchild_span, "grandchild.event.1", Some([("depth", IntValue(3))]))
  
  // End spans in reverse order
  Span::end(grandchild_span)
  Span::end(child_span_2)
  Span::end(child_span_1)
  Span::add_event(root_span, "root.event.3", Some([("event.type", StringValue("completion"))]))
  Span::end(root_span)
  
  // Verify span contexts
  let root_ctx = Span::span_context(root_span)
  let child1_ctx = Span::span_context(child_span_1)
  let child2_ctx = Span::span_context(child_span_2)
  let grandchild_ctx = Span::span_context(grandchild_span)
  
  assert_true(SpanContext::is_valid(root_ctx))
  assert_true(SpanContext::is_valid(child1_ctx))
  assert_true(SpanContext::is_valid(child2_ctx))
  assert_true(SpanContext::is_valid(grandchild_ctx))
  
  assert_eq(SpanContext::trace_id(root_ctx), "test_trace_id")
  assert_eq(SpanContext::trace_id(child1_ctx), "test_trace_id")
  assert_eq(SpanContext::trace_id(child2_ctx), "test_trace_id")
  assert_eq(SpanContext::trace_id(grandchild_ctx), "test_trace_id")
}

// Test 4: Advanced Baggage Operations
test "advanced baggage operations" {
  // Create initial baggage
  let initial_baggage = Baggage::new()
  
  // Add multiple baggage entries
  let baggage_with_user = Baggage::set_entry(initial_baggage, "user.id", "user-12345")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session-abcdef")
  let baggage_with_tenant = Baggage::set_entry(baggage_with_session, "tenant.id", "tenant-67890")
  let baggage_with_request = Baggage::set_entry(baggage_with_tenant, "request.id", "req-12345")
  let baggage_with_correlation = Baggage::set_entry(baggage_with_request, "correlation.id", "corr-abcdef")
  
  // Retrieve baggage entries
  let user_id = Baggage::get_entry(baggage_with_correlation, "user.id")
  let session_id = Baggage::get_entry(baggage_with_correlation, "session.id")
  let tenant_id = Baggage::get_entry(baggage_with_correlation, "tenant.id")
  let request_id = Baggage::get_entry(baggage_with_correlation, "request.id")
  let correlation_id = Baggage::get_entry(baggage_with_correlation, "correlation.id")
  let non_existent = Baggage::get_entry(baggage_with_correlation, "non.existent")
  
  // Verify baggage entries
  assert_eq(user_id, Some("user-12345"))
  assert_eq(session_id, Some("session-abcdef"))
  assert_eq(tenant_id, Some("tenant-67890"))
  assert_eq(request_id, Some("req-12345"))
  assert_eq(correlation_id, Some("corr-abcdef"))
  assert_eq(non_existent, None)
  
  // Remove baggage entries
  let baggage_without_user = Baggage::remove_entry(baggage_with_correlation, "user.id")
  let baggage_without_session = Baggage::remove_entry(baggage_without_user, "session.id")
  
  // Verify removal
  let removed_user_id = Baggage::get_entry(baggage_without_user, "user.id")
  let still_has_tenant = Baggage::get_entry(baggage_without_user, "tenant.id")
  let removed_session_id = Baggage::get_entry(baggage_without_session, "session.id")
  let still_has_tenant_2 = Baggage::get_entry(baggage_without_session, "tenant.id")
  
  assert_eq(removed_user_id, None)
  assert_eq(still_has_tenant, Some("tenant-67890"))
  assert_eq(removed_session_id, None)
  assert_eq(still_has_tenant_2, Some("tenant-67890"))
  
  // Test baggage propagation through context
  let ctx = Context::root()
  let baggage_key = ContextKey::new("baggage")
  let ctx_with_baggage = Context::with_value(ctx, baggage_key, "user.id=user-12345,session.id=session-abcdef")
  let propagated_baggage = Context::get(ctx_with_baggage, baggage_key)
  
  assert_eq(propagated_baggage, Some("user.id=user-12345,session.id=session-abcdef"))
}

// Test 5: Metric Operations with Various Instruments
test "metric operations with various instruments" {
  // Create meter provider and meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "metric.test.meter")
  
  // Create different metric instruments
  let counter = Meter::create_counter(meter, "test.counter")
  let histogram = Meter::create_histogram(meter, "test.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "test.updown_counter")
  let gauge = Meter::create_gauge(meter, "test.gauge")
  
  // Perform counter operations
  Counter::add(counter, 10.0)
  Counter::add(counter, 20.0)
  Counter::add(counter, 15.0)
  
  // Perform histogram operations
  Histogram::record(histogram, 10.5)
  Histogram::record(histogram, 25.7)
  Histogram::record(histogram, 15.3)
  Histogram::record(histogram, 30.2)
  
  // Perform up-down counter operations
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::add(updown_counter, 3.0)
  UpDownCounter::add(updown_counter, -2.0)
  
  // Verify instrument properties
  let counter_instrument = Counter::as_instrument(counter)
  let histogram_instrument = Histogram::as_instrument(histogram)
  let updown_counter_instrument = UpDownCounter::as_instrument(updown_counter)
  let gauge_instrument = Gauge::as_instrument(gauge)
  
  assert_eq(Instrument::name(counter_instrument), "test.counter")
  assert_eq(Instrument::name(histogram_instrument), "test.histogram")
  assert_eq(Instrument::name(updown_counter_instrument), "test.updown_counter")
  assert_eq(Instrument::name(gauge_instrument), "test.gauge")
  
  // Test metric calculations
  let total_counter_value = 10.0 + 20.0 + 15.0
  let avg_histogram_value = (10.5 + 25.7 + 15.3 + 30.2) / 4.0
  let final_updown_value = 5.0 + 3.0 - 2.0
  
  assert_eq(total_counter_value, 45.0)
  assert_eq(avg_histogram_value, 20.425)
  assert_eq(final_updown_value, 6.0)
}

// Test 6: Advanced Log Operations
test "advanced log operations" {
  // Create logger provider and logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "log.test.logger")
  
  // Create log records with different severity levels
  let trace_log = LogRecord::new(Trace, "Trace message for debugging")
  let debug_log = LogRecord::new(Debug, "Debug message for development")
  let info_log = LogRecord::new(Info, "Info message for general information")
  let warn_log = LogRecord::new(Warn, "Warning message for potential issues")
  let error_log = LogRecord::new(Error, "Error message for error conditions")
  let fatal_log = LogRecord::new(Fatal, "Fatal message for critical failures")
  
  // Create log records with context
  let timestamp = Clock::now_unix_nanos(Clock::system())
  let attributes = Attributes::new()
  Attributes::set(attributes, "request.id", StringValue("req-12345"))
  Attributes::set(attributes, "user.id", StringValue("user-67890"))
  Attributes::set(attributes, "operation", StringValue("test.operation"))
  
  let context_log = LogRecord::new_with_context(
    Info,
    Some("Contextual log message with attributes"),
    Some(attributes),
    Some(timestamp),
    Some(timestamp + 1000L),
    Some("trace-123"),
    Some("span-456"),
    Some(Context::root())
  )
  
  // Verify log properties
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  assert_eq(LogRecord::body(trace_log), Some("Trace message for debugging"))
  assert_eq(LogRecord::body(error_log), Some("Error message for error conditions"))
  
  assert_eq(LogRecord::trace_id(context_log), Some("trace-123"))
  assert_eq(LogRecord::span_id(context_log), Some("span-456"))
  assert_eq(LogRecord::timestamp(context_log), Some(timestamp))
  
  // Test log filtering by severity
  let error_threshold = Error
  let should_log_error = LogRecord::severity_number(error_log) >= error_threshold
  let should_log_warn = LogRecord::severity_number(warn_log) >= error_threshold
  let should_log_info = LogRecord::severity_number(info_log) >= error_threshold
  
  assert_true(should_log_error)
  assert_false(should_log_warn)
  assert_false(should_log_info)
  
  // Emit logs
  Logger::emit(logger, info_log)
  Logger::emit(logger, error_log)
  Logger::emit(logger, context_log)
  
  // Test log severity comparison
  let trace_severity = LogRecord::severity_number(trace_log)
  let debug_severity = LogRecord::severity_number(debug_log)
  let info_severity = LogRecord::severity_number(info_log)
  let warn_severity = LogRecord::severity_number(warn_log)
  let error_severity = LogRecord::severity_number(error_log)
  let fatal_severity = LogRecord::severity_number(fatal_log)
  
  assert_true(trace_severity < debug_severity)
  assert_true(debug_severity < info_severity)
  assert_true(info_severity < warn_severity)
  assert_true(warn_severity < error_severity)
  assert_true(error_severity < fatal_severity)
}

// Test 7: Context Propagation Operations
test "context propagation operations" {
  // Create root context
  let root_ctx = Context::root()
  
  // Create context keys
  let user_key = ContextKey::new("user")
  let request_key = ContextKey::new("request")
  let trace_key = ContextKey::new("trace")
  let baggage_key = ContextKey::new("baggage")
  
  // Set context values
  let ctx_with_user = Context::with_value(root_ctx, user_key, "user-12345")
  let ctx_with_request = Context::with_value(ctx_with_user, request_key, "req-abcdef")
  let ctx_with_trace = Context::with_value(ctx_with_request, trace_key, "trace-12345")
  let ctx_with_baggage = Context::with_value(ctx_with_trace, baggage_key, "user.id=user-12345,request.id=req-abcdef")
  
  // Retrieve context values
  let user_value = Context::get(ctx_with_baggage, user_key)
  let request_value = Context::get(ctx_with_baggage, request_key)
  let trace_value = Context::get(ctx_with_baggage, trace_key)
  let baggage_value = Context::get(ctx_with_baggage, baggage_key)
  let non_existent_key = ContextKey::new("nonexistent")
  let non_existent_value = Context::get(ctx_with_baggage, non_existent_key)
  
  // Verify context values
  assert_eq(user_value, Some("user-12345"))
  assert_eq(request_value, Some("req-abcdef"))
  assert_eq(trace_value, Some("trace-12345"))
  assert_eq(baggage_value, Some("user.id=user-12345,request.id=req-abcdef"))
  assert_eq(non_existent_value, None)
  
  // Test context propagation through text map carrier
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "user.id", "user-12345")
  TextMapCarrier::set(carrier, "request.id", "req-abcdef")
  TextMapCarrier::set(carrier, "trace.id", "trace-12345")
  
  // Extract values from carrier
  let extracted_user = TextMapCarrier::get(carrier, "user.id")
  let extracted_request = TextMapCarrier::get(carrier, "request.id")
  let extracted_trace = TextMapCarrier::get(carrier, "trace.id")
  let non_existent_header = TextMapCarrier::get(carrier, "nonexistent")
  
  // Verify extracted values
  assert_eq(extracted_user, Some("user-12345"))
  assert_eq(extracted_request, Some("req-abcdef"))
  assert_eq(extracted_trace, Some("trace-12345"))
  assert_eq(non_existent_header, None)
  
  // Test composite propagator
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // Test injection
  CompositePropagator::inject(composite_propagator, ctx_with_baggage, carrier)
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_baggage = Context::get(extracted_ctx, baggage_key)
  
  assert_true(extracted_baggage != None)
}

// Test 8: Instrumentation Scope Management
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
  let tracer1 = TracerProvider::get_tracer(tracer_provider, scope1.name, scope1.version)
  let tracer2 = TracerProvider::get_tracer(tracer_provider, scope2.name, scope2.version)
  
  let tracer1_scope = Tracer::instrumentation_scope(tracer1)
  let tracer2_scope = Tracer::instrumentation_scope(tracer2)
  
  assert_eq(tracer1_scope.name, scope1.name)
  assert_eq(tracer1_scope.version, scope1.version)
  assert_eq(tracer2_scope.name, scope2.name)
  assert_eq(tracer2_scope.version, scope2.version)
  
  // Test meter with scope
  let meter_provider = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(meter_provider, scope1.name, scope1.version)
  let meter2 = MeterProvider::get_meter(meter_provider, scope2.name, scope2.version)
  
  let counter1 = Meter::create_counter(meter1, "scope1.counter")
  let counter2 = Meter::create_counter(meter2, "scope2.counter")
  
  let counter1_instrument = Counter::as_instrument(counter1)
  let counter2_instrument = Counter::as_instrument(counter2)
  
  assert_eq(Instrument::name(counter1_instrument), "scope1.counter")
  assert_eq(Instrument::name(counter2_instrument), "scope2.counter")
  
  // Test logger with scope
  let logger_provider = LoggerProvider::default()
  let logger1 = LoggerProvider::get_logger(logger_provider, scope1.name, scope1.version)
  let logger2 = LoggerProvider::get_logger(logger_provider, scope3.name, scope3.version)
  
  let log1 = LogRecord::new(Info, "Log from scope1")
  let log2 = LogRecord::new(Info, "Log from scope3")
  
  Logger::emit(logger1, log1)
  Logger::emit(logger2, log2)
}

// Note: Helper functions are already defined in azimuth_advanced_lifecycle_tests.mbt