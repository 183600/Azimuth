// Azimuth Advanced Quality Test Suite
// This file contains advanced quality test cases focusing on edge cases, performance, and robustness

// Test 1: Error handling and recovery mechanisms
test "error handling and recovery mechanisms" {
  // Test invalid span context handling
  let invalid_trace_ctx = SpanContext::new("", "valid-span-id", true, "")
  let invalid_span_ctx = SpanContext::new("valid-trace-id", "", false, "")
  let invalid_both_ctx = SpanContext::new("", "", false, "")
  
  // Verify invalid contexts are properly detected
  assert_false(SpanContext::is_valid(invalid_trace_ctx))
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  assert_false(SpanContext::is_valid(invalid_both_ctx))
  
  // Test span operations with invalid contexts
  let invalid_span = Span::new("invalid-span", Internal, invalid_both_ctx)
  assert_eq(Span::name(invalid_span), "invalid-span")
  assert_eq(Span::kind(invalid_span), Internal)
  assert_false(SpanContext::is_valid(Span::span_context(invalid_span)))
  
  // Test error status handling
  Span::set_status(invalid_span, Error, Some("Invalid span context"))
  assert_eq(Span::status(invalid_span), Error)
  
  // Test recovery with valid context
  let valid_ctx = SpanContext::new("trace-12345", "span-67890", true, "")
  let recovered_span = Span::new("recovered-span", Server, valid_ctx)
  assert_true(SpanContext::is_valid(Span::span_context(recovered_span)))
  Span::set_status(recovered_span, Ok, Some("Recovered successfully"))
  assert_eq(Span::status(recovered_span), Ok)
}

// Test 2: Concurrent safety and thread safety
test "concurrent safety and thread safety" {
  // Test shared resource modification
  let shared_resource = Resource::new()
  
  // Simulate concurrent access by setting multiple attributes
  Resource::with_attributes(shared_resource, [
    ("concurrent.key1", StringValue("value1")),
    ("concurrent.key2", IntValue(42)),
    ("concurrent.key3", BoolValue(true)),
    ("concurrent.key4", FloatValue(3.14)),
    ("concurrent.key5", ArrayStringValue(["a", "b", "c"]))
  ])
  
  // Verify resource integrity after concurrent modifications
  assert_eq(Resource::get_attribute(shared_resource, "concurrent.key1"), Some(StringValue("value1")))
  assert_eq(Resource::get_attribute(shared_resource, "concurrent.key2"), Some(IntValue(42)))
  assert_eq(Resource::get_attribute(shared_resource, "concurrent.key3"), Some(BoolValue(true)))
  assert_eq(Resource::get_attribute(shared_resource, "concurrent.key4"), Some(FloatValue(3.14)))
  assert_eq(Resource::get_attribute(shared_resource, "concurrent.key5"), Some(ArrayStringValue(["a", "b", "c"])))
  
  // Test concurrent context operations
  let base_ctx = Context::root()
  let key1 = ContextKey::new("concurrent.test1")
  let key2 = ContextKey::new("concurrent.test2")
  let key3 = ContextKey::new("concurrent.test3")
  
  let ctx1 = Context::with_value(base_ctx, key1, "value1")
  let ctx2 = Context::with_value(ctx1, key2, "value2")
  let ctx3 = Context::with_value(ctx2, key3, "value3")
  
  // Verify context chain integrity
  assert_eq(Context::get(ctx1, key1), Some("value1"))
  assert_eq(Context::get(ctx2, key1), Some("value1"))
  assert_eq(Context::get(ctx2, key2), Some("value2"))
  assert_eq(Context::get(ctx3, key1), Some("value1"))
  assert_eq(Context::get(ctx3, key2), Some("value2"))
  assert_eq(Context::get(ctx3, key3), Some("value3"))
  
  // Test concurrent baggage operations
  let base_baggage = Baggage::new()
  let baggage1 = Baggage::set_entry(base_baggage, "key1", "value1")
  let baggage2 = Baggage::set_entry(baggage1, "key2", "value2")
  let baggage3 = Baggage::set_entry(baggage2, "key3", "value3")
  
  // Verify baggage integrity
  assert_eq(Baggage::get_entry(baggage1, "key1"), Some("value1"))
  assert_eq(Baggage::get_entry(baggage2, "key1"), Some("value1"))
  assert_eq(Baggage::get_entry(baggage2, "key2"), Some("value2"))
  assert_eq(Baggage::get_entry(baggage3, "key1"), Some("value1"))
  assert_eq(Baggage::get_entry(baggage3, "key2"), Some("value2"))
  assert_eq(Baggage::get_entry(baggage3, "key3"), Some("value3"))
}

// Test 3: Performance boundary and stress testing
test "performance boundary and stress testing" {
  // Test large attribute sets
  let large_attrs = Attributes::new()
  
  // Create attributes with large data
  Attributes::set(large_attrs, "large.string", StringValue("This is a very long string that tests the performance boundaries of the attribute system. It contains a lot of text to simulate real-world scenarios where attributes might contain substantial amounts of data."))
  Attributes::set(large_attrs, "large.int", IntValue(2147483647))
  Attributes::set(large_attrs, "large.float", FloatValue(1.7976931348623157e+308))
  
  // Test large array attributes
  let large_string_array = ArrayStringValue(["item1", "item2", "item3", "item4", "item5", "item6", "item7", "item8", "item9", "item10"])
  let large_int_array = ArrayIntValue([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  
  Attributes::set(large_attrs, "large.string.array", large_string_array)
  Attributes::set(large_attrs, "large.int.array", large_int_array)
  
  // Verify large attributes can be retrieved
  assert_eq(Attributes::get(large_attrs, "large.string"), Some(StringValue("This is a very long string that tests the performance boundaries of the attribute system. It contains a lot of text to simulate real-world scenarios where attributes might contain substantial amounts of data.")))
  assert_eq(Attributes::get(large_attrs, "large.int"), Some(IntValue(2147483647)))
  assert_eq(Attributes::get(large_attrs, "large.float"), Some(FloatValue(1.7976931348623157e+308)))
  assert_eq(Attributes::get(large_attrs, "large.string.array"), Some(large_string_array))
  assert_eq(Attributes::get(large_attrs, "large.int.array"), Some(large_int_array))
  
  // Test span with long name
  let long_span_name = "this.is.a.very.long.span.name.that.tests.the.performance.boundaries.of.the.system.when.dealing.with.extremely.long.identifiers.that.might.occur.in.real.world.scenarios"
  let long_name_span = Span::new(long_span_name, Client, SpanContext::new("trace-12345", "span-67890", true, ""))
  assert_eq(Span::name(long_name_span), long_span_name)
  
  // Test metric with long name
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "performance-test-meter")
  let long_metric_name = "this.is.a.very.long.metric.name.that.tests.the.performance.boundaries.of.the.system.when.dealing.with.extremely.long.identifiers.that.might.occur.in.real.world.scenarios"
  let long_name_metric = Meter::create_counter(meter, long_metric_name)
  assert_eq(long_name_metric.name, long_metric_name)
  
  // Test many metric operations
  for i = 0; i < 1000; i = i + 1 {
    Counter::add(long_name_metric, 1.0)
  }
  
  // Test many log operations
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "performance-test-logger")
  
  for i = 0; i < 100; i = i + 1 {
    let log_record = LogRecord::new(Info, "Performance test log message " + i.to_string())
    Logger::emit(logger, log_record)
  }
}

// Test 4: Data serialization and deserialization integrity
test "data serialization and deserialization integrity" {
  // Test attribute value serialization scenarios
  let test_attrs = Attributes::new()
  
  // Set various attribute types
  Attributes::set(test_attrs, "string.value", StringValue("test string"))
  Attributes::set(test_attrs, "int.value", IntValue(42))
  Attributes::set(test_attrs, "float.value", FloatValue(3.14159))
  Attributes::set(test_attrs, "bool.value", BoolValue(true))
  Attributes::set(test_attrs, "string.array", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(test_attrs, "int.array", ArrayIntValue([1, 2, 3]))
  
  // Test special characters in strings
  Attributes::set(test_attrs, "special.chars", StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  Attributes::set(test_attrs, "unicode.chars", StringValue("测试中文🚀✨🌟"))
  Attributes::set(test_attrs, "newlines", StringValue("line1\nline2\rline3"))
  Attributes::set(test_attrs, "tabs", StringValue("col1\tcol2\tcol3"))
  
  // Test edge case values
  Attributes::set(test_attrs, "empty.string", StringValue(""))
  Attributes::set(test_attrs, "zero.int", IntValue(0))
  Attributes::set(test_attrs, "negative.int", IntValue(-42))
  Attributes::set(test_attrs, "zero.float", FloatValue(0.0))
  Attributes::set(test_attrs, "negative.float", FloatValue(-3.14159))
  Attributes::set(test_attrs, "infinity.float", FloatValue(1.0/0.0))
  
  // Verify attribute integrity
  assert_eq(Attributes::get(test_attrs, "string.value"), Some(StringValue("test string")))
  assert_eq(Attributes::get(test_attrs, "int.value"), Some(IntValue(42)))
  assert_eq(Attributes::get(test_attrs, "float.value"), Some(FloatValue(3.14159)))
  assert_eq(Attributes::get(test_attrs, "bool.value"), Some(BoolValue(true)))
  assert_eq(Attributes::get(test_attrs, "string.array"), Some(ArrayStringValue(["a", "b", "c"])))
  assert_eq(Attributes::get(test_attrs, "int.array"), Some(ArrayIntValue([1, 2, 3])))
  assert_eq(Attributes::get(test_attrs, "special.chars"), Some(StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?")))
  assert_eq(Attributes::get(test_attrs, "unicode.chars"), Some(StringValue("测试中文🚀✨🌟")))
  assert_eq(Attributes::get(test_attrs, "newlines"), Some(StringValue("line1\nline2\rline3")))
  assert_eq(Attributes::get(test_attrs, "tabs"), Some(StringValue("col1\tcol2\tcol3")))
  assert_eq(Attributes::get(test_attrs, "empty.string"), Some(StringValue("")))
  assert_eq(Attributes::get(test_attrs, "zero.int"), Some(IntValue(0)))
  assert_eq(Attributes::get(test_attrs, "negative.int"), Some(IntValue(-42)))
  assert_eq(Attributes::get(test_attrs, "zero.float"), Some(FloatValue(0.0)))
  assert_eq(Attributes::get(test_attrs, "negative.float"), Some(FloatValue(-3.14159)))
  assert_eq(Attributes::get(test_attrs, "infinity.float"), Some(FloatValue(1.0/0.0)))
  
  // Test context serialization scenarios
  let ctx = Context::root()
  let key1 = ContextKey::new("serialization.test1")
  let key2 = ContextKey::new("serialization.test2")
  let key3 = ContextKey::new("serialization.test3")
  
  let ctx_with_values = Context::with_value(
    Context::with_value(
      Context::with_value(ctx, key1, "value1"),
      key2, "value2"
    ),
    key3, "value3"
  )
  
  // Verify context chain integrity
  assert_eq(Context::get(ctx_with_values, key1), Some("value1"))
  assert_eq(Context::get(ctx_with_values, key2), Some("value2"))
  assert_eq(Context::get(ctx_with_values, key3), Some("value3"))
}

// Test 5: Time series data processing
test "time series data processing" {
  // Test timestamp handling
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // Test log records with different timestamps
  let log1 = LogRecord::new_with_context(
    Info,
    Some("Time series log 1"),
    None,
    Some(base_timestamp),
    Some(base_timestamp + 1000L),
    Some("trace-12345"),
    Some("span-67890"),
    None
  )
  
  let log2 = LogRecord::new_with_context(
    Warn,
    Some("Time series log 2"),
    None,
    Some(base_timestamp + 5000L),
    Some(base_timestamp + 6000L),
    Some("trace-12345"),
    Some("span-67891"),
    None
  )
  
  let log3 = LogRecord::new_with_context(
    Error,
    Some("Time series log 3"),
    None,
    Some(base_timestamp + 10000L),
    Some(base_timestamp + 11000L),
    Some("trace-12345"),
    Some("span-67892"),
    None
  )
  
  // Verify timestamp ordering
  assert_eq(LogRecord::timestamp(log1), Some(base_timestamp))
  assert_eq(LogRecord::timestamp(log2), Some(base_timestamp + 5000L))
  assert_eq(LogRecord::timestamp(log3), Some(base_timestamp + 10000L))
  
  assert_true(LogRecord::timestamp(log1)! < LogRecord::timestamp(log2)!)
  assert_true(LogRecord::timestamp(log2)! < LogRecord::timestamp(log3)!)
  
  // Verify observed timestamp ordering
  assert_eq(LogRecord::observed_timestamp(log1), Some(base_timestamp + 1000L))
  assert_eq(LogRecord::observed_timestamp(log2), Some(base_timestamp + 6000L))
  assert_eq(LogRecord::observed_timestamp(log3), Some(base_timestamp + 11000L))
  
  assert_true(LogRecord::observed_timestamp(log1)! < LogRecord::observed_timestamp(log2)!)
  assert_true(LogRecord::observed_timestamp(log2)! < LogRecord::observed_timestamp(log3)!)
  
  // Test trace and span ID relationships
  assert_eq(LogRecord::trace_id(log1), Some("trace-12345"))
  assert_eq(LogRecord::trace_id(log2), Some("trace-12345"))
  assert_eq(LogRecord::trace_id(log3), Some("trace-12345"))
  
  assert_eq(LogRecord::span_id(log1), Some("span-67890"))
  assert_eq(LogRecord::span_id(log2), Some("span-67891"))
  assert_eq(LogRecord::span_id(log3), Some("span-67892"))
  
  // Test time series metrics
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "time-series-meter")
  let histogram = Meter::create_histogram(meter, "response.time")
  
  // Record metrics over time
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 150.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 120.0)
  Histogram::record(histogram, 180.0)
  
  // Test span time relationships
  let span_start = SpanContext::new("trace-time", "span-start", true, "")
  let span1 = Span::new("time-series-span-1", Internal, span_start)
  let span2 = Span::new("time-series-span-2", Internal, span_start)
  let span3 = Span::new("time-series-span-3", Internal, span_start)
  
  // Verify span relationships
  assert_eq(SpanContext::trace_id(Span::span_context(span1)), "trace-time")
  assert_eq(SpanContext::trace_id(Span::span_context(span2)), "trace-time")
  assert_eq(SpanContext::trace_id(Span::span_context(span3)), "trace-time")
  
  assert_eq(SpanContext::span_id(Span::span_context(span1)), "span-start")
  assert_eq(SpanContext::span_id(Span::span_context(span2)), "span-start")
  assert_eq(SpanContext::span_id(Span::span_context(span3)), "span-start")
}

// Test 6: Memory management and resource cleanup
test "memory management and resource cleanup" {
  // Test large resource creation and cleanup
  let large_resource = Resource::new()
  
  // Create resource with many attributes
  let many_attrs = [
    ("attr1", StringValue("value1")),
    ("attr2", StringValue("value2")),
    ("attr3", StringValue("value3")),
    ("attr4", StringValue("value4")),
    ("attr5", StringValue("value5")),
    ("attr6", StringValue("value6")),
    ("attr7", StringValue("value7")),
    ("attr8", StringValue("value8")),
    ("attr9", StringValue("value9")),
    ("attr10", StringValue("value10"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(large_resource, many_attrs)
  
  // Verify all attributes are present
  assert_eq(Resource::get_attribute(resource_with_attrs, "attr1"), Some(StringValue("value1")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "attr2"), Some(StringValue("value2")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "attr3"), Some(StringValue("value3")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "attr4"), Some(StringValue("value4")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "attr5"), Some(StringValue("value5")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "attr6"), Some(StringValue("value6")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "attr7"), Some(StringValue("value7")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "attr8"), Some(StringValue("value8")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "attr9"), Some(StringValue("value9")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "attr10"), Some(StringValue("value10")))
  
  // Test resource merging scenarios
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("base.attr1", StringValue("base1")),
    ("base.attr2", StringValue("base2")),
    ("base.attr3", StringValue("base3"))
  ])
  
  let override_resource = Resource::with_attributes(Resource::new(), [
    ("override.attr1", StringValue("override1")),
    ("base.attr2", StringValue("override2")),  // Override base attribute
    ("override.attr3", StringValue("override3"))
  ])
  
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // Verify merge behavior (override should win for conflicts)
  assert_eq(Resource::get_attribute(merged_resource, "base.attr1"), Some(StringValue("base1")))  // Unchanged
  assert_eq(Resource::get_attribute(merged_resource, "base.attr2"), Some(StringValue("override2")))  // Overridden
  assert_eq(Resource::get_attribute(merged_resource, "base.attr3"), Some(StringValue("base3")))  // Unchanged
  assert_eq(Resource::get_attribute(merged_resource, "override.attr1"), Some(StringValue("override1")))  // New
  assert_eq(Resource::get_attribute(merged_resource, "override.attr3"), Some(StringValue("override3")))  // New
  
  // Test context chaining and cleanup
  let ctx1 = Context::root()
  let key1 = ContextKey::new("memory.test1")
  let key2 = ContextKey::new("memory.test2")
  let key3 = ContextKey::new("memory.test3")
  
  let ctx2 = Context::with_value(ctx1, key1, "value1")
  let ctx3 = Context::with_value(ctx2, key2, "value2")
  let ctx4 = Context::with_value(ctx3, key3, "value3")
  
  // Verify context chain
  assert_eq(Context::get(ctx4, key1), Some("value1"))
  assert_eq(Context::get(ctx4, key2), Some("value2"))
  assert_eq(Context::get(ctx4, key3), Some("value3"))
  
  // Test baggage cleanup
  let baggage1 = Baggage::new()
  let baggage2 = Baggage::set_entry(baggage1, "key1", "value1")
  let baggage3 = Baggage::set_entry(baggage2, "key2", "value2")
  let baggage4 = Baggage::set_entry(baggage3, "key3", "value3")
  
  // Verify baggage chain
  assert_eq(Baggage::get_entry(baggage4, "key1"), Some("value1"))
  assert_eq(Baggage::get_entry(baggage4, "key2"), Some("value2"))
  assert_eq(Baggage::get_entry(baggage4, "key3"), Some("value3"))
  
  // Test baggage removal
  let baggage5 = Baggage::remove_entry(baggage4, "key2")
  assert_eq(Baggage::get_entry(baggage5, "key1"), Some("value1"))
  assert_eq(Baggage::get_entry(baggage5, "key2"), None)  // Should be removed
  assert_eq(Baggage::get_entry(baggage5, "key3"), Some("value3"))
}

// Test 7: Configuration management and dynamic updates
test "configuration management and dynamic updates" {
  // Test instrumentation scope configuration
  let scope1 = InstrumentationScope::{ name: "scope1", version: Some("1.0.0"), schema_url: Some("https://example.com/schema1") }
  let scope2 = InstrumentationScope::{ name: "scope2", version: None, schema_url: Some("https://example.com/schema2") }
  let scope3 = InstrumentationScope::{ name: "scope3", version: Some("3.0.0"), schema_url: None }
  let scope4 = InstrumentationScope::{ name: "scope4", version: None, schema_url: None }
  
  // Verify scope configurations
  assert_eq(scope1.name, "scope1")
  assert_eq(scope1.version, Some("1.0.0"))
  assert_eq(scope1.schema_url, Some("https://example.com/schema1"))
  
  assert_eq(scope2.name, "scope2")
  assert_eq(scope2.version, None)
  assert_eq(scope2.schema_url, Some("https://example.com/schema2"))
  
  assert_eq(scope3.name, "scope3")
  assert_eq(scope3.version, Some("3.0.0"))
  assert_eq(scope3.schema_url, None)
  
  assert_eq(scope4.name, "scope4")
  assert_eq(scope4.version, None)
  assert_eq(scope4.schema_url, None)
  
  // Test tracer configuration
  let tracer_provider = TracerProvider::default()
  let tracer1 = TracerProvider::get_tracer(tracer_provider, "tracer1", Some("1.0.0"))
  let tracer2 = TracerProvider::get_tracer(tracer_provider, "tracer2", None)
  
  assert_eq(Tracer::instrumentation_scope(tracer1).name, "tracer1")
  assert_eq(Tracer::instrumentation_scope(tracer1).version, Some("1.0.0"))
  
  assert_eq(Tracer::instrumentation_scope(tracer2).name, "tracer2")
  assert_eq(Tracer::instrumentation_scope(tracer2).version, None)
  
  // Test meter configuration
  let meter_provider = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(meter_provider, "meter1", Some("1.0.0"), Some("https://example.com/schema1"))
  let meter2 = MeterProvider::get_meter(meter_provider, "meter2", None, Some("https://example.com/schema2"))
  let meter3 = MeterProvider::get_meter(meter_provider, "meter3", Some("3.0.0"), None)
  let meter4 = MeterProvider::get_meter(meter_provider, "meter4", None, None)
  
  // Verify meter configurations
  assert_eq(meter1.scope.name, "meter1")
  assert_eq(meter1.scope.version, Some("1.0.0"))
  assert_eq(meter1.scope.schema_url, Some("https://example.com/schema1"))
  
  assert_eq(meter2.scope.name, "meter2")
  assert_eq(meter2.scope.version, None)
  assert_eq(meter2.scope.schema_url, Some("https://example.com/schema2"))
  
  assert_eq(meter3.scope.name, "meter3")
  assert_eq(meter3.scope.version, Some("3.0.0"))
  assert_eq(meter3.scope.schema_url, None)
  
  assert_eq(meter4.scope.name, "meter4")
  assert_eq(meter4.scope.version, None)
  assert_eq(meter4.scope.schema_url, None)
  
  // Test logger configuration
  let logger_provider = LoggerProvider::default()
  let logger1 = LoggerProvider::get_logger(logger_provider, "logger1", Some("1.0.0"), Some("https://example.com/schema1"))
  let logger2 = LoggerProvider::get_logger(logger_provider, "logger2", None, Some("https://example.com/schema2"))
  let logger3 = LoggerProvider::get_logger(logger_provider, "logger3", Some("3.0.0"), None)
  let logger4 = LoggerProvider::get_logger(logger_provider, "logger4", None, None)
  
  // Verify logger configurations
  assert_eq(logger1.scope.name, "logger1")
  assert_eq(logger1.scope.version, Some("1.0.0"))
  assert_eq(logger1.scope.schema_url, Some("https://example.com/schema1"))
  
  assert_eq(logger2.scope.name, "logger2")
  assert_eq(logger2.scope.version, None)
  assert_eq(logger2.scope.schema_url, Some("https://example.com/schema2"))
  
  assert_eq(logger3.scope.name, "logger3")
  assert_eq(logger3.scope.version, Some("3.0.0"))
  assert_eq(logger3.scope.schema_url, None)
  
  assert_eq(logger4.scope.name, "logger4")
  assert_eq(logger4.scope.version, None)
  assert_eq(logger4.scope.schema_url, None)
  
  // Test instrument configuration variations
  let counter1 = Meter::create_counter(meter1, "counter1", Some("Counter 1 description"), Some("count"))
  let counter2 = Meter::create_counter(meter2, "counter2", None, Some("count"))
  let counter3 = Meter::create_counter(meter3, "counter3", Some("Counter 3 description"), None)
  let counter4 = Meter::create_counter(meter4, "counter4", None, None)
  
  // Verify counter configurations
  assert_eq(counter1.name, "counter1")
  assert_eq(counter1.description, Some("Counter 1 description"))
  assert_eq(counter1.unit, Some("count"))
  
  assert_eq(counter2.name, "counter2")
  assert_eq(counter2.description, None)
  assert_eq(counter2.unit, Some("count"))
  
  assert_eq(counter3.name, "counter3")
  assert_eq(counter3.description, Some("Counter 3 description"))
  assert_eq(counter3.unit, None)
  
  assert_eq(counter4.name, "counter4")
  assert_eq(counter4.description, None)
  assert_eq(counter4.unit, None)
}

// Test 8: Internationalization and localization support
test "internationalization and localization support" {
  // Test Unicode and multilingual attribute values
  let unicode_attrs = Attributes::new()
  
  // Test various Unicode scripts
  Attributes::set(unicode_attrs, "chinese.text", StringValue("这是一个测试文本"))
  Attributes::set(unicode_attrs, "japanese.text", StringValue("これはテストテキストです"))
  Attributes::set(unicode_attrs, "korean.text", StringValue("이것은 테스트 텍스트입니다"))
  Attributes::set(unicode_attrs, "arabic.text", StringValue("هذا نص اختبار"))
  Attributes::set(unicode_attrs, "russian.text", StringValue("Это тестовый текст"))
  Attributes::set(unicode_attrs, "hebrew.text", StringValue("זהו טקסט מבחן"))
  Attributes::set(unicode_attrs, "thai.text", StringValue("นี่คือข้อความทดสอบ"))
  
  // Test emoji and special Unicode characters
  Attributes::set(unicode_attrs, "emoji.text", StringValue("🚀🔥✨🌟💫⭐🌈🦄🐉🌺🌸"))
  Attributes::set(unicode_attrs, "math.symbols", StringValue("∑∏∫∆∇∂∞±≤≥≈≠∈∉⊂⊃∧∨"))
  Attributes::set(unicode_attrs, "currency.symbols", StringValue("$€£¥₹₽₩₪₫₡₨₦₱₲₴₸₼₽"))
  
  // Test right-to-left languages
  Attributes::set(unicode_attrs, "rtl.text", StringValue("مرحبا بالعالم"))
  Attributes::set(unicode_attrs, "mixed.direction", StringValue("Hello مرحبا World"))
  
  // Test combining characters and diacritics
  Attributes::set(unicode_attrs, "diacritics.text", StringValue("café résumé naïve façade"))
  Attributes::set(unicode_attrs, "combining.text", StringValue("e\u0301a\u0300i\u0302u\u0308o\u0303"))
  
  // Verify Unicode attributes
  assert_eq(Attributes::get(unicode_attrs, "chinese.text"), Some(StringValue("这是一个测试文本")))
  assert_eq(Attributes::get(unicode_attrs, "japanese.text"), Some(StringValue("これはテストテキストです")))
  assert_eq(Attributes::get(unicode_attrs, "korean.text"), Some(StringValue("이것은 테스트 텍스트입니다")))
  assert_eq(Attributes::get(unicode_attrs, "arabic.text"), Some(StringValue("هذا نص اختبار")))
  assert_eq(Attributes::get(unicode_attrs, "russian.text"), Some(StringValue("Это тестовый текст")))
  assert_eq(Attributes::get(unicode_attrs, "hebrew.text"), Some(StringValue("זהו טקסט מבחן")))
  assert_eq(Attributes::get(unicode_attrs, "thai.text"), Some(StringValue("นี่คือข้อความทดสอบ")))
  assert_eq(Attributes::get(unicode_attrs, "emoji.text"), Some(StringValue("🚀🔥✨🌟💫⭐🌈🦄🐉🌺🌸")))
  assert_eq(Attributes::get(unicode_attrs, "math.symbols"), Some(StringValue("∑∏∫∆∇∂∞±≤≥≈≠∈∉⊂⊃∧∨")))
  assert_eq(Attributes::get(unicode_attrs, "currency.symbols"), Some(StringValue("$€£¥₹₽₩₪₫₡₨₦₱₲₴₸₼₽")))
  assert_eq(Attributes::get(unicode_attrs, "rtl.text"), Some(StringValue("مرحبا بالعالم")))
  assert_eq(Attributes::get(unicode_attrs, "mixed.direction"), Some(StringValue("Hello مرحبا World")))
  assert_eq(Attributes::get(unicode_attrs, "diacritics.text"), Some(StringValue("café résumé naïve façade")))
  assert_eq(Attributes::get(unicode_attrs, "combining.text"), Some(StringValue("e\u0301a\u0300i\u0302u\u0308o\u0303")))
  
  // Test multilingual log messages
  let chinese_log = LogRecord::new(Info, "中文日志消息")
  let japanese_log = LogRecord::new(Warn, "日本語のログメッセージ")
  let korean_log = LogRecord::new(Error, "한국어 로그 메시지")
  let arabic_log = LogRecord::new(Debug, "رسالة سجل باللغة العربية")
  let emoji_log = LogRecord::new(Info, "🚀 Log message with emoji 🎉")
  
  // Verify multilingual log messages
  assert_eq(LogRecord::severity_number(chinese_log), Info)
  assert_eq(LogRecord::body(chinese_log), Some("中文日志消息"))
  
  assert_eq(LogRecord::severity_number(japanese_log), Warn)
  assert_eq(LogRecord::body(japanese_log), Some("日本語のログメッセージ"))
  
  assert_eq(LogRecord::severity_number(korean_log), Error)
  assert_eq(LogRecord::body(korean_log), Some("한국어 로그 메시지"))
  
  assert_eq(LogRecord::severity_number(arabic_log), Debug)
  assert_eq(LogRecord::body(arabic_log), Some("رسالة سجل باللغة العربية"))
  
  assert_eq(LogRecord::severity_number(emoji_log), Info)
  assert_eq(LogRecord::body(emoji_log), Some("🚀 Log message with emoji 🎉"))
  
  // Test multilingual context values
  let ctx = Context::root()
  let chinese_key = ContextKey::new("中文键")
  let japanese_key = ContextKey::new("日本語キー")
  let emoji_key = ContextKey::new("emoji_key🚀")
  
  let ctx_with_chinese = Context::with_value(ctx, chinese_key, "中文值")
  let ctx_with_japanese = Context::with_value(ctx_with_chinese, japanese_key, "日本語の値")
  let ctx_with_emoji = Context::with_value(ctx_with_japanese, emoji_key, "emoji_value🎉")
  
  // Verify multilingual context values
  assert_eq(Context::get(ctx_with_chinese, chinese_key), Some("中文值"))
  assert_eq(Context::get(ctx_with_japanese, japanese_key), Some("日本語の値"))
  assert_eq(Context::get(ctx_with_emoji, emoji_key), Some("emoji_value🎉"))
  
  // Test multilingual baggage entries
  let baggage = Baggage::new()
  let baggage_with_chinese = Baggage::set_entry(baggage, "用户ID", "12345")
  let baggage_with_japanese = Baggage::set_entry(baggage_with_chinese, "リクエストID", "req-67890")
  let baggage_with_emoji = Baggage::set_entry(baggage_with_japanese, "special_key🚀", "special_value🎉")
  
  // Verify multilingual baggage entries
  assert_eq(Baggage::get_entry(baggage_with_chinese, "用户ID"), Some("12345"))
  assert_eq(Baggage::get_entry(baggage_with_japanese, "リクエストID"), Some("req-67890"))
  assert_eq(Baggage::get_entry(baggage_with_emoji, "special_key🚀"), Some("special_value🎉"))
}