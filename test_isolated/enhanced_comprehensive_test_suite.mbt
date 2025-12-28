// Enhanced Test Suite for Azimuth Telemetry System
// Comprehensive test coverage for advanced scenarios and edge cases

test "error_handling_and_recovery" {
  // Test error handling in span operations
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "error_test_tracer")
  
  // Test span with invalid context
  let invalid_span_ctx = SpanContext::new("", "", false, "")
  assert_eq(SpanContext::is_valid(invalid_span_ctx), false)
  
  // Test span creation with error recovery
  let span = Tracer::start_span(tracer, "error_test_span")
  Span::set_status(span, Error)
  
  // Test log record with error severity
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Critical system error"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("error_trace_id"),
    Some("error_span_id"),
    None
  )
  
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::body(error_log), Some("Critical system error"))
  assert_eq(LogRecord::trace_id(error_log), Some("error_trace_id"))
}

test "boundary_conditions_and_extreme_values" {
  // Test with empty strings and null values
  let empty_attrs = Attributes::new()
  assert_eq(Attributes::get(empty_attrs, "non.existent.key"), None)
  
  // Test with maximum values
  let max_int_attr = IntValue(2147483647)
  let min_int_attr = IntValue(-2147483648)
  let max_float_attr = FloatValue(1.7976931348623157e+308)
  
  // Test with very long strings
  let long_string = "a" * 1000
  let long_string_attr = StringValue(long_string)
  
  match long_string_attr {
    StringValue(s) => assert_eq(@string.length(s), 1000)
    _ => @test.fail("Expected StringValue with long string")
  }
  
  // Test context with extreme key lengths
  let very_long_key = "k" * 500
  let long_key_ctx = ContextKey::new(very_long_key)
  let root_ctx = Context::root()
  let ctx_with_long_key = Context::with_value(root_ctx, long_key_ctx, "test_value")
  
  assert_eq(Context::get(ctx_with_long_key, long_key_ctx), Some("test_value"))
  
  // Test span context with edge case IDs
  let edge_trace_id = "0" * 32
  let edge_span_id = "f" * 16
  let edge_ctx = SpanContext::new(edge_trace_id, edge_span_id, true, "")
  
  assert_eq(SpanContext::trace_id(edge_ctx), edge_trace_id)
  assert_eq(SpanContext::span_id(edge_ctx), edge_span_id)
  assert_eq(SpanContext::is_valid(edge_ctx), true)
}

test "performance_and_large_data_handling" {
  // Test handling of large attribute arrays
  let large_string_array = ArrayStringValue(["item" + @string.to_string(i) for i in range(0, 100)])
  let large_int_array = ArrayIntValue([i for i in range(0, 100)])
  
  match large_string_array {
    ArrayStringValue(arr) => assert_eq(@array.length(arr), 100)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match large_int_array {
    ArrayIntValue(arr) => assert_eq(@array.length(arr), 100)
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  // Test resource with many attributes
  let many_attrs = [
    ("attr." + @string.to_string(i), StringValue("value" + @string.to_string(i)))
    for i in range(0, 50)
  ]
  
  let resource_with_many_attrs = Resource::with_attributes(Resource::new(), many_attrs)
  assert_eq(Resource::get_attribute(resource_with_many_attrs, "attr.0"), Some(StringValue("value0")))
  assert_eq(Resource::get_attribute(resource_with_many_attrs, "attr.49"), Some(StringValue("value49")))
  
  // Test baggage with many entries
  let baggage = Baggage::new()
  let baggage_with_many = Baggage::set_entry(baggage, "key.1", "value.1")
  assert_eq(Baggage::get_entry(baggage_with_many, "key.1"), Some("value.1"))
}

test "component_integration_workflow" {
  // Test complete telemetry workflow
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "integration_tracer", Some("1.0.0"))
  let meter = MeterProvider::get_meter(meter_provider, "integration_meter")
  let logger = LoggerProvider::get_logger(logger_provider, "integration_logger")
  
  // Create span
  let span = Tracer::start_span(tracer, "integration_operation")
  Span::add_event(span, "operation_started", None)
  
  // Create metrics
  let counter = Meter::create_counter(meter, "operation_counter", Some("Operation counter"), Some("operations"))
  Counter::add(counter, 1.0, None)
  
  let histogram = Meter::create_histogram(meter, "operation_duration", Some("Operation duration"), Some("ms"))
  Histogram::record(histogram, 150.5, None)
  
  // Create log record
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Integration operation completed"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(Span::span_context(span).trace_id),
    Some(Span::span_context(span).span_id),
    None
  )
  
  // Verify integration
  assert_eq(Span::name(span), "integration_operation")
  assert_eq(Instrument::name(Counter(counter.name, counter.description, counter.unit)), "operation_counter")
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(LogRecord::trace_id(log_record), Some(Span::span_context(span).trace_id))
  
  Span::end(span)
}

test "data_serialization_integrity" {
  // Test attribute value serialization scenarios
  let complex_attrs = [
    ("string.attr", StringValue("test with spaces & symbols!@#$%^&*()")),
    ("int.attr", IntValue(-999999)),
    ("float.attr", FloatValue(3.14159265359)),
    ("bool.attr", BoolValue(false)),
    ("array.string", ArrayStringValue(["中文", "English", "Español", "Français"])),
    ("array.int", ArrayIntValue([-1, 0, 1, 999, -999]))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), complex_attrs)
  
  // Verify data integrity
  assert_eq(Resource::get_attribute(resource, "string.attr"), Some(StringValue("test with spaces & symbols!@#$%^&*()")))
  assert_eq(Resource::get_attribute(resource, "int.attr"), Some(IntValue(-999999)))
  assert_eq(Resource::get_attribute(resource, "float.attr"), Some(FloatValue(3.14159265359)))
  assert_eq(Resource::get_attribute(resource, "bool.attr"), Some(BoolValue(false)))
  
  match Resource::get_attribute(resource, "array.string") {
    Some(ArrayStringValue(arr)) => assert_eq(@array.length(arr), 4)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match Resource::get_attribute(resource, "array.int") {
    Some(ArrayIntValue(arr)) => assert_eq(@array.length(arr), 5)
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  // Test context data integrity
  let ctx_key = ContextKey::new("serialization.test")
  let ctx_value = "test value with unicode: 🚀 🌟 💫"
  let ctx = Context::with_value(Context::root(), ctx_key, ctx_value)
  
  assert_eq(Context::get(ctx, ctx_key), Some(ctx_value))
}

test "resource_management_and_lifecycle" {
  // Test resource creation and management
  let base_resource = Resource::new()
  let service_attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.namespace", StringValue("production"))
  ]
  
  let service_resource = Resource::with_attributes(base_resource, service_attrs)
  
  // Test resource merging
  let override_attrs = [
    ("service.version", StringValue("3.0.0")),
    ("deployment.environment", StringValue("staging"))
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(service_resource, override_resource)
  
  // Verify merge behavior
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("test-service")))
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), Some(StringValue("3.0.0")))
  assert_eq(Resource::get_attribute(merged_resource, "deployment.environment"), Some(StringValue("staging")))
  
  // Test instrumentation scope lifecycle
  let scope = InstrumentationScope::{ 
    name: "test.scope", 
    version: Some("1.2.3"), 
    schema_url: Some("https://example.com/schema/v1") 
  }
  
  assert_eq(scope.name, "test.scope")
  assert_eq(scope.version, Some("1.2.3"))
  assert_eq(scope.schema_url, Some("https://example.com/schema/v1"))
}

test "cross_service_propagation_comprehensive" {
  // Test comprehensive cross-service propagation
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // Create source context
  let source_key = ContextKey::new("user.id")
  let source_ctx = Context::with_value(Context::root(), source_key, "user-12345")
  
  // Create carrier for propagation
  let carrier = TextMapCarrier::new()
  
  // Inject context into carrier
  CompositePropagator::inject(composite, source_ctx, carrier)
  
  // Simulate cross-service call headers
  TextMapCarrier::set(carrier, "x-request-id", "req-789")
  TextMapCarrier::set(carrier, "x-b3-traceid", "trace-abc")
  TextMapCarrier::set(carrier, "x-b3-spanid", "span-def")
  TextMapCarrier::set(carrier, "x-b3-sampled", "1")
  
  // Extract context in destination service
  let dest_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_key = ContextKey::new("extracted")
  
  assert_eq(Context::get(dest_ctx, extracted_key), Some("true"))
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(TextMapCarrier::get(carrier, "x-request-id"), Some("req-789"))
  
  // Test baggage propagation
  let baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage, "user.role", "admin")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "sess-456")
  
  assert_eq(Baggage::get_entry(baggage_with_session, "user.role"), Some("admin"))
  assert_eq(Baggage::get_entry(baggage_with_session, "session.id"), Some("sess-456"))
}

test "internationalization_and_unicode_support" {
  // Test internationalization support with various character sets
  let i18n_attrs = [
    ("service.name.zh", StringValue("遥测服务")),
    ("service.name.ja", StringValue("テレメトリーサービス")),
    ("service.name.ko", StringValue("원격 측정 서비스")),
    ("service.name.ar", StringValue("خدمة القياس عن بعد")),
    ("service.name.ru", StringValue("Служба телеизмерений")),
    ("service.name.es", StringValue("Servicio de telemetría")),
    ("service.name.fr", StringValue("Service de télémétrie")),
    ("service.name.de", StringValue("Telemetriedienst"))
  ]
  
  let i18n_resource = Resource::with_attributes(Resource::new(), i18n_attrs)
  
  // Verify internationalization attributes
  assert_eq(Resource::get_attribute(i18n_resource, "service.name.zh"), Some(StringValue("遥测服务")))
  assert_eq(Resource::get_attribute(i18n_resource, "service.name.ja"), Some(StringValue("テレメトリーサービス")))
  assert_eq(Resource::get_attribute(i18n_resource, "service.name.ko"), Some(StringValue("원격 측정 서비스")))
  assert_eq(Resource::get_attribute(i18n_resource, "service.name.ar"), Some(StringValue("خدمة القياس عن بعد")))
  assert_eq(Resource::get_attribute(i18n_resource, "service.name.ru"), Some(StringValue("Служба телеизмерений")))
  
  // Test Unicode in log messages
  let unicode_log = LogRecord::new_with_context(
    Info,
    Some("Unicode test: 🌍 🚀 📊 测试中文 тест русский العربية 한국어 日本語"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("unicode_trace_🚀"),
    Some("unicode_span_📊"),
    None
  )
  
  assert_eq(LogRecord::severity_number(unicode_log), Info)
  assert_eq(LogRecord::trace_id(unicode_log), Some("unicode_trace_🚀"))
  assert_eq(LogRecord::span_id(unicode_log), Some("unicode_span_📊"))
  
  // Test context with Unicode keys and values
  let unicode_key = ContextKey::new("ユーザー.名前")
  let unicode_value = "田中太郎"
  let unicode_ctx = Context::with_value(Context::root(), unicode_key, unicode_value)
  
  assert_eq(Context::get(unicode_ctx, unicode_key), Some(unicode_value))
  
  // Test baggage with Unicode content
  let unicode_baggage = Baggage::new()
  let baggage_with_unicode = Baggage::set_entry(unicode_baggage, "utilisateur.nom", "Jean-François")
  
  assert_eq(Baggage::get_entry(baggage_with_unicode, "utilisateur.nom"), Some("Jean-François"))
}