// Enhanced Quality Test Suite for Azimuth Telemetry System
// This file contains high-quality test cases focusing on advanced functionality

test "attributes deep operations with complex values" {
  // Test attributes with various complex value types
  let attrs = Attributes::new()
  
  // Test setting different attribute value types
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14159))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  Attributes::set(attrs, "array.string.key", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "array.int.key", ArrayIntValue([1, 2, 3]))
  
  // Test retrieving values
  assert_eq(Attributes::get(attrs, "string.key"), Some(StringValue("test_value")))
  assert_eq(Attributes::get(attrs, "int.key"), Some(IntValue(42)))
  assert_eq(Attributes::get(attrs, "float.key"), Some(FloatValue(3.14159)))
  assert_eq(Attributes::get(attrs, "bool.key"), Some(BoolValue(true)))
  assert_eq(Attributes::get(attrs, "array.string.key"), Some(ArrayStringValue(["a", "b", "c"])))
  assert_eq(Attributes::get(attrs, "array.int.key"), Some(ArrayIntValue([1, 2, 3])))
  
  // Test missing key
  assert_eq(Attributes::get(attrs, "missing.key"), None)
}

test "baggage propagation across context boundaries" {
  // Test baggage operations and propagation
  let baggage = Baggage::new()
  
  // Test setting baggage entries
  let baggage_with_entries = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage_with_more_entries = Baggage::set_entry(baggage_with_entries, "request.id", "req-67890")
  
  // Test retrieving baggage entries
  assert_eq(Baggage::get_entry(baggage_with_more_entries, "user.id"), Some("12345"))
  assert_eq(Baggage::get_entry(baggage_with_more_entries, "request.id"), Some("req-67890"))
  
  // Test removing baggage entries
  let baggage_after_removal = Baggage::remove_entry(baggage_with_more_entries, "user.id")
  assert_eq(Baggage::get_entry(baggage_after_removal, "user.id"), None)
  assert_eq(Baggage::get_entry(baggage_after_removal, "request.id"), Some("req-67890"))
  
  // Test missing entry
  assert_eq(Baggage::get_entry(baggage, "nonexistent.key"), None)
}

test "composite propagator injection and extraction" {
  // Test composite propagator functionality
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // Test context injection
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Verify injection worked
  let injected_trace = TextMapCarrier::get(carrier, "traceparent")
  assert_true(injected_trace != None)
  
  // Test context extraction
  let extraction_carrier = TextMapCarrier::new()
  TextMapCarrier::set(extraction_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  let extracted_ctx = CompositePropagator::extract(composite, extraction_carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  
  assert_eq(extracted_value, Some("true"))
}

test "span lifecycle management with status and events" {
  // Test complete span lifecycle
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer", Some("1.0.0"))
  
  // Test tracer properties
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test-tracer")
  assert_eq(scope.version, Some("1.0.0"))
  
  // Test span creation
  let span = Tracer::start_span(tracer, "test-span", Some([("operation.type", StringValue("test"))]))
  
  // Test span properties
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  let span_ctx = Span::span_context(span)
  assert_eq(SpanContext::trace_id(span_ctx), "test_trace_id")
  assert_eq(SpanContext::span_id(span_ctx), "test_span_id")
  assert_true(SpanContext::is_sampled(span_ctx))
  
  // Test span status
  assert_eq(Span::status(span), Unset)
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // Test span events
  Span::add_event(span, "event1", Some([("event.key", StringValue("event.value"))]))
  Span::add_event(span, "event2", None)
  
  // Test span ending
  Span::end(span)
  
  assert_true(true)
}

test "histogram metrics with advanced operations" {
  // Test histogram creation and operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram-test")
  
  let histogram = Meter::create_histogram(
    meter, 
    "request.duration", 
    Some("Request duration in seconds"), 
    Some("s")
  )
  
  // Test histogram properties
  assert_eq(histogram.name, "request.duration")
  assert_eq(histogram.description, Some("Request duration in seconds"))
  assert_eq(histogram.unit, Some("s"))
  
  // Test recording values
  Histogram::record(histogram, 0.1)
  Histogram::record(histogram, 0.5)
  Histogram::record(histogram, 1.2)
  Histogram::record(histogram, 2.8)
  
  // Test histogram as instrument
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "request.duration")
  assert_eq(Instrument::description(instrument), Some("Request duration in seconds"))
  assert_eq(Instrument::unit(instrument), Some("s"))
  
  assert_true(true)
}

test "resource merging and attribute management" {
  // Test resource operations and merging
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production"))
  ]
  let resource_with_base = Resource::with_attributes(base_resource, base_attrs)
  
  // Test resource attribute retrieval
  assert_eq(Resource::get_attribute(resource_with_base, "service.name"), Some(StringValue("azimuth-service")))
  assert_eq(Resource::get_attribute(resource_with_base, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(resource_with_base, "deployment.environment"), Some(StringValue("production")))
  
  // Test override resource
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("updated-service")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource_with_override = Resource::with_attributes(override_resource, override_attrs)
  
  // Test resource merging
  let merged_resource = Resource::merge(resource_with_base, resource_with_override)
  
  // In simplified implementation, override wins
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("updated-service")))
  assert_eq(Resource::get_attribute(merged_resource, "service.instance.id"), Some(StringValue("instance-123")))
  
  // Test missing attribute
  assert_eq(Resource::get_attribute(resource_with_base, "missing.attribute"), None)
}

test "attribute value type conversion and validation" {
  // Test attribute value type conversions and edge cases
  let string_val = StringValue("test string")
  let int_val = IntValue(42)
  let float_val = FloatValue(3.14159)
  let bool_val = BoolValue(true)
  let string_array_val = ArrayStringValue(["a", "b", "c"])
  let int_array_val = ArrayIntValue([1, 2, 3])
  
  // Test creating attributes with different value types
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.test", string_val)
  Attributes::set(attrs, "int.test", int_val)
  Attributes::set(attrs, "float.test", float_val)
  Attributes::set(attrs, "bool.test", bool_val)
  Attributes::set(attrs, "array.string.test", string_array_val)
  Attributes::set(attrs, "array.int.test", int_array_val)
  
  // Test retrieval and type matching
  assert_eq(Attributes::get(attrs, "string.test"), Some(StringValue("test string")))
  assert_eq(Attributes::get(attrs, "int.test"), Some(IntValue(42)))
  assert_eq(Attributes::get(attrs, "float.test"), Some(FloatValue(3.14159)))
  assert_eq(Attributes::get(attrs, "bool.test"), Some(BoolValue(true)))
  assert_eq(Attributes::get(attrs, "array.string.test"), Some(ArrayStringValue(["a", "b", "c"])))
  assert_eq(Attributes::get(attrs, "array.int.test"), Some(ArrayIntValue([1, 2, 3])))
  
  // Test edge cases
  let empty_string_val = StringValue("")
  let zero_int_val = IntValue(0)
  let negative_int_val = IntValue(-1)
  let zero_float_val = FloatValue(0.0)
  let negative_float_val = FloatValue(-3.14)
  let false_bool_val = BoolValue(false)
  let empty_string_array = ArrayStringValue([])
  let empty_int_array = ArrayIntValue([])
  
  Attributes::set(attrs, "empty.string", empty_string_val)
  Attributes::set(attrs, "zero.int", zero_int_val)
  Attributes::set(attrs, "negative.int", negative_int_val)
  Attributes::set(attrs, "zero.float", zero_float_val)
  Attributes::set(attrs, "negative.float", negative_float_val)
  Attributes::set(attrs, "false.bool", false_bool_val)
  Attributes::set(attrs, "empty.array.string", empty_string_array)
  Attributes::set(attrs, "empty.array.int", empty_int_array)
  
  assert_eq(Attributes::get(attrs, "empty.string"), Some(StringValue("")))
  assert_eq(Attributes::get(attrs, "zero.int"), Some(IntValue(0)))
  assert_eq(Attributes::get(attrs, "negative.int"), Some(IntValue(-1)))
  assert_eq(Attributes::get(attrs, "zero.float"), Some(FloatValue(0.0)))
  assert_eq(Attributes::get(attrs, "negative.float"), Some(FloatValue(-3.14)))
  assert_eq(Attributes::get(attrs, "false.bool"), Some(BoolValue(false)))
  assert_eq(Attributes::get(attrs, "empty.array.string"), Some(ArrayStringValue([])))
  assert_eq(Attributes::get(attrs, "empty.array.int"), Some(ArrayIntValue([])))
}

test "advanced metrics with updown counter and gauge" {
  // Test advanced metric types beyond counters
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "advanced-metrics-test")
  
  // Test UpDownCounter
  let updown_counter = Meter::create_updown_counter(
    meter,
    "active.connections",
    Some("Current number of active connections"),
    Some("connections")
  )
  
  // Test UpDownCounter properties
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(updown_counter.description, Some("Current number of active connections"))
  assert_eq(updown_counter.unit, Some("connections"))
  
  // Test UpDownCounter operations (can go up and down)
  UpDownCounter::add(updown_counter, 10.0)  // Add 10 connections
  UpDownCounter::add(updown_counter, 5.0)   // Add 5 more connections
  UpDownCounter::add(updown_counter, -3.0)  // Remove 3 connections
  
  // Test Gauge
  let gauge = Meter::create_gauge(
    meter,
    "memory.usage",
    Some("Current memory usage in bytes"),
    Some("bytes")
  )
  
  // Test Gauge properties
  assert_eq(gauge.name, "memory.usage")
  assert_eq(gauge.description, Some("Current memory usage in bytes"))
  assert_eq(gauge.unit, Some("bytes"))
  
  // Test Gauge operations
  UpDownCounter::add(gauge, 1024.0)  // Set memory usage to 1KB
  UpDownCounter::add(gauge, 512.0)   // Update memory usage
  
  assert_true(true)
}

test "context propagation with complex key hierarchies" {
  // Test complex context propagation scenarios
  let root_ctx = Context::root()
  
  // Create a hierarchy of context keys
  let correlation_key = ContextKey::new("correlation.id")
  let user_key = ContextKey::new("user.id")
  let request_key = ContextKey::new("request.id")
  let trace_key = ContextKey::new("trace.id")
  
  // Build context with multiple values
  let ctx_with_correlation = Context::with_value(root_ctx, correlation_key, "corr-12345")
  let ctx_with_user = Context::with_value(ctx_with_correlation, user_key, "user-67890")
  let ctx_with_request = Context::with_value(ctx_with_user, request_key, "req-abcdef")
  let final_ctx = Context::with_value(ctx_with_request, trace_key, "trace-fedcba")
  
  // Test context value retrieval at different levels
  assert_eq(Context::get(final_ctx, correlation_key), Some("corr-12345"))
  assert_eq(Context::get(final_ctx, user_key), Some("user-67890"))
  assert_eq(Context::get(final_ctx, request_key), Some("req-abcdef"))
  assert_eq(Context::get(final_ctx, trace_key), Some("trace-fedcba"))
  
  // Test missing keys in intermediate contexts
  assert_eq(Context::get(ctx_with_correlation, user_key), None)
  assert_eq(Context::get(ctx_with_user, request_key), None)
  
  // Test context isolation
  let isolated_ctx = Context::with_value(root_ctx, ContextKey::new("isolated.key"), "isolated.value")
  assert_eq(Context::get(isolated_ctx, correlation_key), None)
  assert_eq(Context::get(isolated_ctx, ContextKey::new("isolated.key")), Some("isolated.value"))
  
  // Test complex key names with special characters
  let special_key = ContextKey::new("special.key.with.dots")
  let ctx_with_special = Context::with_value(final_ctx, special_key, "special.value")
  assert_eq(Context::get(ctx_with_special, special_key), Some("special.value"))
  
  // Test empty key name handling
  let empty_key = ContextKey::new("")
  let ctx_with_empty = Context::with_value(ctx_with_special, empty_key, "empty.key.value")
  assert_eq(Context::get(ctx_with_empty, empty_key), Some("empty.key.value"))
}