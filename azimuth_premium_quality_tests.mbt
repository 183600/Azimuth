// Enhanced Premium Quality Telemetry Tests for Azimuth System
// High-quality test cases covering advanced telemetry scenarios

test "span lifecycle and attribute management" {
  // Test span creation, attribute management, and lifecycle
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "test.service")
  
  // Create span with initial attributes
  let span = Tracer::start_span(tracer, "operation.test")
  assert_eq(Span::name(span), "operation.test")
  assert_true(Span::is_recording(span))
  
  // Add attributes to span
  Span::set_attribute(span, "user.id", StringValue("user-123"))
  Span::set_attribute(span, "operation.type", StringValue("critical"))
  Span::set_attribute(span, "retry.count", IntValue(3))
  Span::set_attribute(span, "cache.enabled", BoolValue(true))
  
  // Verify attributes
  assert_eq(Span::get_attribute(span, "user.id"), Some(StringValue("user-123")))
  assert_eq(Span::get_attribute(span, "operation.type"), Some(StringValue("critical")))
  assert_eq(Span::get_attribute(span, "retry.count"), Some(IntValue(3)))
  assert_eq(Span::get_attribute(span, "cache.enabled"), Some(BoolValue(true)))
  
  // Add events to span
  Span::add_event(span, "database.query", Some([("query", StringValue("SELECT * FROM users"))]))
  Span::add_event(span, "cache.miss", Some([("cache.key", StringValue("user:123"))]))
  
  // Set span status and end
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  Span::end(span)
  assert_false(Span::is_recording(span))
}

test "metrics counter and histogram operations" {
  // Test counter and histogram metric operations
  let provider = MeterProvider::new()
  let meter = MeterProvider::get_meter(provider, "metrics.test")
  
  // Create counter metric
  let request_counter = Meter::create_counter(
    meter, 
    "http.requests.total", 
    Some("Total HTTP requests"), 
    Some("requests")
  )
  
  // Record counter values with attributes
  Counter::add(request_counter, 1, Some([("method", StringValue("GET")), ("endpoint", StringValue("/api/users"))]))
  Counter::add(request_counter, 1, Some([("method", StringValue("POST")), ("endpoint", StringValue("/api/users"))]))
  Counter::add(request_counter, 1, Some([("method", StringValue("GET")), ("endpoint", StringValue("/api/orders"))]))
  
  // Verify counter values
  assert_eq(Counter::value(request_counter), 3)
  
  // Create histogram metric
  let response_histogram = Meter::create_histogram(
    meter,
    "http.request.duration",
    Some("HTTP request duration histogram"),
    Some("milliseconds")
  )
  
  // Record histogram values
  Histogram::record(response_histogram, 100.0, Some([("endpoint", StringValue("/api/users"))]))
  Histogram::record(response_histogram, 150.0, Some([("endpoint", StringValue("/api/users"))]))
  Histogram::record(response_histogram, 200.0, Some([("endpoint", StringValue("/api/orders"))]))
  
  // Verify histogram statistics
  let count = Histogram::count(response_histogram)
  let sum = Histogram::sum(response_histogram)
  let mean = Histogram::mean(response_histogram)
  
  assert_eq(count, 3)
  assert_eq(sum, 450.0)
  assert_eq(mean, 150.0)
}

test "baggage operations and propagation" {
  // Test baggage operations and propagation mechanisms
  let baggage = Baggage::new()
  
  // Set baggage entries
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "user-789")
  let updated_baggage = Baggage::set_entry(updated_baggage, "request.id", "req-12345")
  let updated_baggage = Baggage::set_entry(updated_baggage, "tenant.id", "tenant-001")
  
  // Verify baggage entries
  assert_eq(Baggage::get_entry(updated_baggage, "user.id"), Some("user-789"))
  assert_eq(Baggage::get_entry(updated_baggage, "request.id"), Some("req-12345"))
  assert_eq(Baggage::get_entry(updated_baggage, "tenant.id"), Some("tenant-001"))
  
  // Test baggage removal
  let filtered_baggage = Baggage::remove_entry(updated_baggage, "request.id")
  assert_eq(Baggage::get_entry(filtered_baggage, "user.id"), Some("user-789"))
  assert_eq(Baggage::get_entry(filtered_baggage, "request.id"), None)
  assert_eq(Baggage::get_entry(filtered_baggage, "tenant.id"), Some("tenant-001"))
  
  // Test baggage serialization
  let serialized = Baggage::to_string(filtered_baggage)
  assert_true(serialized.contains("user.id=user-789"))
  assert_true(serialized.contains("tenant.id=tenant-001"))
  assert_false(serialized.contains("request.id"))
  
  // Test baggage deserialization
  let deserialized = Baggage::from_string(serialized)
  assert_eq(Baggage::get_entry(deserialized, "user.id"), Some("user-789"))
  assert_eq(Baggage::get_entry(deserialized, "tenant.id"), Some("tenant-001"))
}

test "context propagation with text map carrier" {
  // Test context propagation using text map carrier
  let carrier = TextMapCarrier::new()
  
  // Create context with trace information
  let context = Context::new()
  let context = Context::with_value(context, ContextKey::new("trace.id"), "trace-12345")
  let context = Context::with_value(context, ContextKey::new("span.id"), "span-67890")
  
  // Create propagator
  let propagator = W3CTraceContextPropagator::new()
  
  // Inject context into carrier
  CompositePropagator::inject(propagator, context, carrier)
  
  // Verify injected headers
  assert_true(TextMapCarrier::contains(carrier, "traceparent"))
  assert_true(TextMapCarrier::contains(carrier, "tracestate"))
  
  // Extract context from carrier
  let extracted_context = CompositePropagator::extract(propagator, carrier)
  
  // Verify extracted context
  assert_eq(Context::get_value(extracted_context, ContextKey::new("trace.id")), Some("trace-12345"))
  assert_eq(Context::get_value(extracted_context, ContextKey::new("span.id")), Some("span-67890"))
}

test "resource attributes and merging" {
  // Test resource attributes and merging strategies
  let resource1 = Resource::new([
    ("service.name", StringValue("service-a")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("staging"))
  ])
  
  let resource2 = Resource::new([
    ("service.name", StringValue("service-b")),  // This should override
    ("host.name", StringValue("host-001")),
    ("service.instance.id", StringValue("instance-123"))
  ])
  
  // Merge resources (resource2 should take precedence)
  let merged = Resource::merge(resource1, resource2)
  
  // Verify merged attributes
  assert_eq(Resource::get_attribute(merged, "service.name"), Some(StringValue("service-b")))
  assert_eq(Resource::get_attribute(merged, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(merged, "deployment.environment"), Some(StringValue("staging")))
  assert_eq(Resource::get_attribute(merged, "host.name"), Some(StringValue("host-001")))
  assert_eq(Resource::get_attribute(merged, "service.instance.id"), Some(StringValue("instance-123")))
  
  // Test resource filtering
  let filtered = Resource::filter(merged, fn(key, value) {
    key.starts_with("service.")
  })
  
  assert_true(Resource::has_attribute(filtered, "service.name"))
  assert_true(Resource::has_attribute(filtered, "service.version"))
  assert_true(Resource::has_attribute(filtered, "service.instance.id"))
  assert_false(Resource::has_attribute(filtered, "host.name"))
  assert_false(Resource::has_attribute(filtered, "deployment.environment"))
}

test "span relationships and event correlation" {
  // Test span relationships and event correlation
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "relationship.test")
  
  // Create parent span
  let parent_span = Tracer::start_span(tracer, "parent.operation")
  Span::set_attribute(parent_span, "operation.type", StringValue("parent"))
  
  // Create child span
  let child_span = Tracer::start_span_with_parent(tracer, "child.operation", parent_span)
  Span::set_attribute(child_span, "operation.type", StringValue("child"))
  
  // Add events to child span
  Span::add_event(child_span, "event.a", Some([("step", IntValue(1))]))
  Span::add_event(child_span, "event.b", Some([("step", IntValue(2))]))
  
  // Create another child span
  let child_span2 = Tracer::start_span_with_parent(tracer, "child.operation2", parent_span)
  Span::set_attribute(child_span2, "operation.type", StringValue("child2"))
  
  // Add events to second child span
  Span::add_event(child_span2, "event.c", Some([("step", IntValue(3))]))
  
  // End child spans first
  Span::end(child_span)
  Span::end(child_span2)
  
  // End parent span
  Span::end(parent_span)
  
  // Verify span relationships
  let parent_context = Span::context(parent_span)
  let child_context = Span::context(child_span)
  let child_context2 = Span::context(child_span2)
  
  assert_eq(SpanContext::trace_id(parent_context), SpanContext::trace_id(child_context))
  assert_eq(SpanContext::trace_id(parent_context), SpanContext::trace_id(child_context2))
  assert_eq(SpanContext::parent_span_id(child_context), SpanContext::span_id(parent_context))
  assert_eq(SpanContext::parent_span_id(child_context2), SpanContext::span_id(parent_context))
}

test "attribute value type conversions" {
  // Test attribute value type conversions and operations
  let attributes = Attributes::new()
  
  // Add different types of attributes
  Attributes::set(attributes, "string.attr", StringValue("test.value"))
  Attributes::set(attributes, "int.attr", IntValue(42))
  Attributes::set(attributes, "float.attr", FloatValue(3.14))
  Attributes::set(attributes, "bool.attr", BoolValue(true))
  
  // Verify attribute values
  assert_eq(Attributes::get(attributes, "string.attr"), Some(StringValue("test.value")))
  assert_eq(Attributes::get(attributes, "int.attr"), Some(IntValue(42)))
  assert_eq(Attributes::get(attributes, "float.attr"), Some(FloatValue(3.14)))
  assert_eq(Attributes::get(attributes, "bool.attr"), Some(BoolValue(true)))
  
  // Test attribute type conversions
  match Attributes::get(attributes, "int.attr") {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  match Attributes::get(attributes, "float.attr") {
    Some(FloatValue(value)) => assert_true(value > 3.0 && value < 3.2)
    _ => assert_true(false)
  }
  
  // Test array attributes
  Attributes::set(attributes, "array.string", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attributes, "array.int", ArrayIntValue([1, 2, 3]))
  
  match Attributes::get(attributes, "array.string") {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "a")
      assert_eq(values[1], "b")
      assert_eq(values[2], "c")
    }
    _ => assert_true(false)
  }
  
  match Attributes::get(attributes, "array.int") {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], 1)
      assert_eq(values[1], 2)
      assert_eq(values[2], 3)
    }
    _ => assert_true(false)
  }
}

test "span status and error handling" {
  // Test span status management and error handling
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "status.test")
  
  // Create span for successful operation
  let success_span = Tracer::start_span(tracer, "successful.operation")
  Span::set_attribute(success_span, "operation.result", StringValue("success"))
  Span::set_status(success_span, Ok, Some("Operation completed successfully"))
  Span::end(success_span)
  
  // Verify success span status
  assert_eq(Span::status(success_span), Ok)
  
  // Create span for failed operation
  let error_span = Tracer::start_span(tracer, "failed.operation")
  Span::set_attribute(error_span, "operation.result", StringValue("error"))
  Span::set_status(error_span, Error, Some("Operation failed with timeout"))
  Span::end(error_span)
  
  // Verify error span status
  assert_eq(Span::status(error_span), Error)
  
  // Create span with error events
  let exception_span = Tracer::start_span(tracer, "exception.operation")
  Span::add_event(exception_span, "exception", Some([
    ("exception.type", StringValue("TimeoutException")),
    ("exception.message", StringValue("Operation timed out after 30 seconds")),
    ("exception.stacktrace", StringValue("at Service.operation() line 42"))
  ]))
  Span::set_status(exception_span, Error, Some("Operation failed with exception"))
  Span::end(exception_span)
  
  // Verify exception span status
  assert_eq(Span::status(exception_span), Error)
  
  // Test span with unset status
  let unset_span = Tracer::start_span(tracer, "unset.operation")
  Span::end(unset_span)
  
  // Verify unset span status
  assert_eq(Span::status(unset_span), Unset)
}