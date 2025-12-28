// Configuration Dynamic Update Tests
// Testing dynamic configuration updates and their impact on telemetry components

test "dynamic tracer provider configuration update" {
  // Test updating tracer provider configuration dynamically
  let initial_provider = TracerProvider::default()
  let initial_tracer = TracerProvider::get_tracer(initial_provider, "initial-tracer", Some("1.0.0"))
  let initial_scope = Tracer::instrumentation_scope(initial_tracer)
  
  // Verify initial configuration
  assert_eq(initial_scope.name, "initial-tracer")
  assert_eq(initial_scope.version, Some("1.0.0"))
  
  // Simulate configuration update
  let updated_provider = TracerProvider::default()
  let updated_tracer = TracerProvider::get_tracer(updated_provider, "updated-tracer", Some("2.0.0"))
  let updated_scope = Tracer::instrumentation_scope(updated_tracer)
  
  // Verify updated configuration
  assert_eq(updated_scope.name, "updated-tracer")
  assert_eq(updated_scope.version, Some("2.0.0"))
  
  // Verify configuration change
  assert_true(initial_scope.name != updated_scope.name)
  assert_true(initial_scope.version != updated_scope.version)
}

test "dynamic meter provider configuration update" {
  // Test updating meter provider configuration dynamically
  let initial_provider = MeterProvider::default()
  let initial_meter = MeterProvider::get_meter(initial_provider, "initial-meter")
  let initial_counter = Meter::create_counter(initial_meter, "initial.counter", Some("Initial counter"), Some("count"))
  
  // Verify initial configuration
  assert_eq(initial_counter.name, "initial.counter")
  assert_eq(initial_counter.description, Some("Initial counter"))
  
  // Simulate configuration update
  let updated_provider = MeterProvider::default()
  let updated_meter = MeterProvider::get_meter(updated_provider, "updated-meter")
  let updated_counter = Meter::create_counter(updated_meter, "updated.counter", Some("Updated counter"), Some("requests"))
  
  // Verify updated configuration
  assert_eq(updated_counter.name, "updated.counter")
  assert_eq(updated_counter.description, Some("Updated counter"))
  assert_eq(updated_counter.unit, Some("requests"))
  
  // Verify configuration change
  assert_true(initial_counter.name != updated_counter.name)
  assert_true(initial_counter.description != updated_counter.description)
  assert_true(initial_counter.unit != updated_counter.unit)
}

test "dynamic logger provider configuration update" {
  // Test updating logger provider configuration dynamically
  let initial_provider = LoggerProvider::default()
  let initial_logger = LoggerProvider::get_logger(initial_provider, "initial-logger")
  let initial_record = LogRecord::new(Info, "Initial log message")
  
  // Verify initial configuration
  assert_eq(LogRecord::severity_number(initial_record), Info)
  assert_eq(LogRecord::body(initial_record), Some("Initial log message"))
  
  // Simulate configuration update
  let updated_provider = LoggerProvider::default()
  let updated_logger = LoggerProvider::get_logger(updated_provider, "updated-logger")
  let updated_record = LogRecord::new(Warn, "Updated log message")
  
  // Verify updated configuration
  assert_eq(LogRecord::severity_number(updated_record), Warn)
  assert_eq(LogRecord::body(updated_record), Some("Updated log message"))
  
  // Verify configuration change
  assert_true(LogRecord::severity_number(initial_record) != LogRecord::severity_number(updated_record))
  assert_true(LogRecord::body(initial_record) != LogRecord::body(updated_record))
}

test "dynamic resource configuration update" {
  // Test updating resource configuration dynamically
  let initial_attrs = [
    ("service.name", StringValue("initial-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("development"))
  ]
  let initial_resource = Resource::with_attributes(Resource::new(), initial_attrs)
  
  // Verify initial configuration
  let initial_service_name = Resource::get_attribute(initial_resource, "service.name")
  let initial_version = Resource::get_attribute(initial_resource, "service.version")
  let initial_env = Resource::get_attribute(initial_resource, "deployment.environment")
  
  assert_eq(initial_service_name, Some(StringValue("initial-service")))
  assert_eq(initial_version, Some(StringValue("1.0.0")))
  assert_eq(initial_env, Some(StringValue("development")))
  
  // Simulate configuration update
  let updated_attrs = [
    ("service.name", StringValue("updated-service")),
    ("service.version", StringValue("2.0.0")),
    ("deployment.environment", StringValue("production"))
  ]
  let updated_resource = Resource::with_attributes(Resource::new(), updated_attrs)
  
  // Verify updated configuration
  let updated_service_name = Resource::get_attribute(updated_resource, "service.name")
  let updated_version = Resource::get_attribute(updated_resource, "service.version")
  let updated_env = Resource::get_attribute(updated_resource, "deployment.environment")
  
  assert_eq(updated_service_name, Some(StringValue("updated-service")))
  assert_eq(updated_version, Some(StringValue("2.0.0")))
  assert_eq(updated_env, Some(StringValue("production")))
  
  // Verify configuration change
  assert_true(initial_service_name != updated_service_name)
  assert_true(initial_version != updated_version)
  assert_true(initial_env != updated_env)
}

test "dynamic propagator configuration update" {
  // Test updating propagator configuration dynamically
  let initial_propagator = W3CTraceContextPropagator::new()
  let initial_composite = CompositePropagator::new([initial_propagator])
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test initial configuration
  CompositePropagator::inject(initial_composite, ctx, carrier)
  let initial_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(initial_traceparent, Some("00-test-trace-id-test-span-id-01"))
  
  // Simulate configuration update with multiple propagators
  let updated_propagator1 = W3CTraceContextPropagator::new()
  let updated_propagator2 = W3CTraceContextPropagator::new()
  let updated_composite = CompositePropagator::new([updated_propagator1, updated_propagator2])
  
  let updated_carrier = TextMapCarrier::new()
  CompositePropagator::inject(updated_composite, ctx, updated_carrier)
  let updated_traceparent = TextMapCarrier::get(updated_carrier, "traceparent")
  assert_eq(updated_traceparent, Some("00-test-trace-id-test-span-id-01"))
}

test "dynamic span configuration update" {
  // Test updating span configuration dynamically
  let initial_span_ctx = SpanContext::new("trace-123", "span-456", true, "")
  let initial_span = Span::new("initial-operation", Internal, initial_span_ctx)
  
  // Verify initial configuration
  assert_eq(Span::name(initial_span), "initial-operation")
  assert_eq(Span::kind(initial_span), Internal)
  assert_true(Span::is_recording(initial_span))
  
  // Simulate configuration update
  let updated_span_ctx = SpanContext::new("trace-123", "span-789", true, "")
  let updated_span = Span::new("updated-operation", Server, updated_span_ctx)
  
  // Verify updated configuration
  assert_eq(Span::name(updated_span), "updated-operation")
  assert_eq(Span::kind(updated_span), Server)
  assert_true(Span::is_recording(updated_span))
  
  // Verify configuration change
  assert_true(Span::name(initial_span) != Span::name(updated_span))
  assert_true(Span::kind(initial_span) != Span::kind(updated_span))
  assert_true(SpanContext::span_id(initial_span_ctx) != SpanContext::span_id(updated_span_ctx))
}

test "dynamic metric instrument configuration update" {
  // Test updating metric instrument configuration dynamically
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Initial instrument configuration
  let initial_counter = Meter::create_counter(meter, "http.requests", Some("HTTP requests"), Some("count"))
  let initial_histogram = Meter::create_histogram(meter, "response.time", Some("Response time"), Some("ms"))
  
  // Verify initial configuration
  assert_eq(initial_counter.name, "http.requests")
  assert_eq(initial_counter.description, Some("HTTP requests"))
  assert_eq(initial_counter.unit, Some("count"))
  
  assert_eq(initial_histogram.name, "response.time")
  assert_eq(initial_histogram.description, Some("Response time"))
  assert_eq(initial_histogram.unit, Some("ms"))
  
  // Simulate configuration update
  let updated_counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let updated_histogram = Meter::create_histogram(meter, "http.response.time", Some("HTTP response time"), Some("milliseconds"))
  
  // Verify updated configuration
  assert_eq(updated_counter.name, "http.requests.total")
  assert_eq(updated_counter.description, Some("Total HTTP requests"))
  assert_eq(updated_counter.unit, Some("requests"))
  
  assert_eq(updated_histogram.name, "http.response.time")
  assert_eq(updated_histogram.description, Some("HTTP response time"))
  assert_eq(updated_histogram.unit, Some("milliseconds"))
  
  // Verify configuration change
  assert_true(initial_counter.name != updated_counter.name)
  assert_true(initial_counter.description != updated_counter.description)
  assert_true(initial_counter.unit != updated_counter.unit)
  
  assert_true(initial_histogram.name != updated_histogram.name)
  assert_true(initial_histogram.description != updated_histogram.description)
  assert_true(initial_histogram.unit != updated_histogram.unit)
}

test "dynamic baggage configuration update" {
  // Test updating baggage configuration dynamically
  let initial_baggage = Baggage::new()
  
  // Set initial baggage entries
  let initial_with_entries = Baggage::set_entry(initial_baggage, "user.id", "12345")
  let initial_with_entries = Baggage::set_entry(initial_with_entries, "session.id", "sess-abc")
  
  // Verify initial configuration
  let initial_user_id = Baggage::get_entry(initial_with_entries, "user.id")
  let initial_session_id = Baggage::get_entry(initial_with_entries, "session.id")
  
  assert_eq(initial_user_id, Some("12345"))
  assert_eq(initial_session_id, Some("sess-abc"))
  
  // Simulate configuration update
  let updated_baggage = Baggage::new()
  let updated_with_entries = Baggage::set_entry(updated_baggage, "user.id", "67890")
  let updated_with_entries = Baggage::set_entry(updated_with_entries, "request.id", "req-def")
  
  // Verify updated configuration
  let updated_user_id = Baggage::get_entry(updated_with_entries, "user.id")
  let updated_request_id = Baggage::get_entry(updated_with_entries, "request.id")
  
  assert_eq(updated_user_id, Some("67890"))
  assert_eq(updated_request_id, Some("req-def"))
  
  // Verify configuration change
  assert_true(initial_user_id != updated_user_id)
  assert_true(initial_session_id != updated_request_id)
}

test "dynamic context configuration update" {
  // Test updating context configuration dynamically
  let initial_ctx = Context::root()
  let initial_key = ContextKey::new("initial.key")
  let initial_with_value = Context::with_value(initial_ctx, initial_key, "initial.value")
  
  // Verify initial configuration
  let initial_value = Context::get(initial_with_value, initial_key)
  assert_eq(initial_value, Some("initial.value"))
  
  // Simulate configuration update
  let updated_ctx = Context::root()
  let updated_key = ContextKey::new("updated.key")
  let updated_with_value = Context::with_value(updated_ctx, updated_key, "updated.value")
  
  // Verify updated configuration
  let updated_value = Context::get(updated_with_value, updated_key)
  assert_eq(updated_value, Some("updated.value"))
  
  // Verify configuration change
  assert_true(initial_key.key != updated_key.key)
  assert_true(initial_value != updated_value)
}

test "dynamic configuration update with complex attributes" {
  // Test dynamic updates with complex attribute types
  let initial_attrs = [
    ("string.attr", StringValue("initial string")),
    ("int.attr", IntValue(100)),
    ("float.attr", FloatValue(1.5)),
    ("bool.attr", BoolValue(true)),
    ("array.string", ArrayStringValue(["initial", "array"])),
    ("array.int", ArrayIntValue([1, 2, 3]))
  ]
  let initial_resource = Resource::with_attributes(Resource::new(), initial_attrs)
  
  // Verify initial configuration
  assert_eq(Resource::get_attribute(initial_resource, "string.attr"), Some(StringValue("initial string")))
  assert_eq(Resource::get_attribute(initial_resource, "int.attr"), Some(IntValue(100)))
  assert_eq(Resource::get_attribute(initial_resource, "float.attr"), Some(FloatValue(1.5)))
  assert_eq(Resource::get_attribute(initial_resource, "bool.attr"), Some(BoolValue(true)))
  
  // Simulate configuration update
  let updated_attrs = [
    ("string.attr", StringValue("updated string")),
    ("int.attr", IntValue(200)),
    ("float.attr", FloatValue(2.5)),
    ("bool.attr", BoolValue(false)),
    ("array.string", ArrayStringValue(["updated", "array", "values"])),
    ("array.int", ArrayIntValue([4, 5, 6, 7]))
  ]
  let updated_resource = Resource::with_attributes(Resource::new(), updated_attrs)
  
  // Verify updated configuration
  assert_eq(Resource::get_attribute(updated_resource, "string.attr"), Some(StringValue("updated string")))
  assert_eq(Resource::get_attribute(updated_resource, "int.attr"), Some(IntValue(200)))
  assert_eq(Resource::get_attribute(updated_resource, "float.attr"), Some(FloatValue(2.5)))
  assert_eq(Resource::get_attribute(updated_resource, "bool.attr"), Some(BoolValue(false)))
  
  // Verify configuration change
  assert_true(Resource::get_attribute(initial_resource, "string.attr") != Resource::get_attribute(updated_resource, "string.attr"))
  assert_true(Resource::get_attribute(initial_resource, "int.attr") != Resource::get_attribute(updated_resource, "int.attr"))
  assert_true(Resource::get_attribute(initial_resource, "float.attr") != Resource::get_attribute(updated_resource, "float.attr"))
  assert_true(Resource::get_attribute(initial_resource, "bool.attr") != Resource::get_attribute(updated_resource, "bool.attr"))
}

test "dynamic configuration update rollback" {
  // Test rolling back configuration updates
  let original_attrs = [
    ("service.name", StringValue("original-service")),
    ("service.version", StringValue("1.0.0"))
  ]
  let original_resource = Resource::with_attributes(Resource::new(), original_attrs)
  
  // Apply update
  let updated_attrs = [
    ("service.name", StringValue("updated-service")),
    ("service.version", StringValue("2.0.0"))
  ]
  let updated_resource = Resource::with_attributes(Resource::new(), updated_attrs)
  
  // Verify update applied
  assert_eq(Resource::get_attribute(updated_resource, "service.name"), Some(StringValue("updated-service")))
  assert_eq(Resource::get_attribute(updated_resource, "service.version"), Some(StringValue("2.0.0")))
  
  // Rollback to original configuration
  let rollback_resource = Resource::with_attributes(Resource::new(), original_attrs)
  
  // Verify rollback successful
  assert_eq(Resource::get_attribute(rollback_resource, "service.name"), Some(StringValue("original-service")))
  assert_eq(Resource::get_attribute(rollback_resource, "service.version"), Some(StringValue("1.0.0")))
  
  // Verify rollback matches original
  assert_eq(Resource::get_attribute(original_resource, "service.name"), Resource::get_attribute(rollback_resource, "service.name"))
  assert_eq(Resource::get_attribute(original_resource, "service.version"), Resource::get_attribute(rollback_resource, "service.version"))
}