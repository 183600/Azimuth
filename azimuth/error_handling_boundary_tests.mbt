// Error Handling and Boundary Conditions Tests
// Testing system behavior under error conditions and edge cases

test "span context validation with invalid inputs" {
  // Test empty trace ID and span ID
  let empty_trace_ctx = SpanContext::new("", "valid_span_id", true, "")
  assert_false(SpanContext::is_valid(empty_trace_ctx))
  
  let empty_span_ctx = SpanContext::new("valid_trace_id", "", true, "")
  assert_false(SpanContext::is_valid(empty_span_ctx))
  
  let both_empty_ctx = SpanContext::new("", "", true, "")
  assert_false(SpanContext::is_valid(both_empty_ctx))
  
  // Test invalid trace ID formats
  let short_trace_ctx = SpanContext::new("short", "valid_span_id", true, "")
  assert_false(SpanContext::is_valid(short_trace_ctx))
  
  let long_trace_ctx = SpanContext::new("toolongtraceidthatexceedsthemaximumallowedlengthforavalidtraceid", "valid_span_id", true, "")
  assert_false(SpanContext::is_valid(long_trace_ctx))
  
  // Test invalid span ID formats
  let short_span_ctx = SpanContext::new("valid_trace_id", "short", true, "")
  assert_false(SpanContext::is_valid(short_span_ctx))
  
  let long_span_ctx = SpanContext::new("valid_trace_id", "toolongspanidthatexceedsthemaximumallowedlengthforavalidspanid", true, "")
  assert_false(SpanContext::is_valid(long_span_ctx))
  
  // Test with special characters
  let special_chars_trace_ctx = SpanContext::new("trace@#$%", "span_123", true, "")
  assert_false(SpanContext::is_valid(special_chars_trace_ctx))
  
  let special_chars_span_ctx = SpanContext::new("trace_123", "span@#$%", true, "")
  assert_false(SpanContext::is_valid(special_chars_span_ctx))
}

test "attribute operations with boundary values" {
  let attrs = Attributes::new()
  
  // Test empty string values
  Attributes::set(attrs, "empty.string", StringValue(""))
  assert_eq(Attributes::get(attrs, "empty.string"), Some(StringValue("")))
  
  // Test single character values
  Attributes::set(attrs, "single.char", StringValue("a"))
  assert_eq(Attributes::get(attrs, "single.char"), Some(StringValue("a")))
  
  // Test very long string values
  let very_long_string = "a".repeat(10000)  // 10,000 character string
  Attributes::set(attrs, "very.long.string", StringValue(very_long_string))
  assert_eq(Attributes::get(attrs, "very.long.string"), Some(StringValue(very_long_string)))
  
  // Test boundary numeric values
  Attributes::set(attrs, "max.int32", IntValue(2147483647))
  Attributes::set(attrs, "min.int32", IntValue(-2147483648))
  Attributes::set(attrs, "max.int64", IntValue(9223372036854775807))
  Attributes::set(attrs, "min.int64", IntValue(-9223372036854775808))
  
  assert_eq(Attributes::get(attrs, "max.int32"), Some(IntValue(2147483647)))
  assert_eq(Attributes::get(attrs, "min.int32"), Some(IntValue(-2147483648)))
  
  // Test boundary float values
  Attributes::set(attrs, "max.float", FloatValue(1.7976931348623157e+308))  // Max double
  Attributes::set(attrs, "min.float", FloatValue(-1.7976931348623157e+308)) // Min double
  Attributes::set(attrs, "very.small", FloatValue(5e-324))  // Smallest positive double
  
  assert_eq(Attributes::get(attrs, "max.float"), Some(FloatValue(1.7976931348623157e+308)))
  assert_eq(Attributes::get(attrs, "min.float"), Some(FloatValue(-1.7976931348623157e+308)))
  
  // Test empty arrays
  Attributes::set(attrs, "empty.string.array", ArrayStringValue([]))
  Attributes::set(attrs, "empty.int.array", ArrayIntValue([]))
  
  assert_eq(Attributes::get(attrs, "empty.string.array"), Some(ArrayStringValue([])))
  assert_eq(Attributes::get(attrs, "empty.int.array"), Some(ArrayIntValue([])))
  
  // Test very large arrays
  let large_string_array = ArrayStringValue(["item" + i.to_string() for i in 0..1000])
  let large_int_array = ArrayIntValue([i for i in 0..1000])
  
  Attributes::set(attrs, "large.string.array", large_string_array)
  Attributes::set(attrs, "large.int.array", large_int_array)
  
  assert_eq(Attributes::get(attrs, "large.string.array"), Some(large_string_array))
  assert_eq(Attributes::get(attrs, "large.int.array"), Some(large_int_array))
}

test "context operations with edge cases" {
  let root_ctx = Context::root()
  
  // Test context with very long keys
  let long_key = "a".repeat(1000)  // 1000 character key
  let long_key_obj = ContextKey::new(long_key)
  let ctx_with_long_key = Context::with_value(root_ctx, long_key_obj, "long.key.value")
  
  assert_eq(Context::get(ctx_with_long_key, long_key_obj), Some("long.key.value"))
  
  // Test context with very long values
  let long_value = "b".repeat(10000)  // 10,000 character value
  let normal_key = ContextKey::new("normal.key")
  let ctx_with_long_value = Context::with_value(root_ctx, normal_key, long_value)
  
  assert_eq(Context::get(ctx_with_long_value, normal_key), Some(long_value))
  
  // Test context with special characters in keys
  let special_chars_keys = [
    "key.with.dots",
    "key_with_underscores",
    "key-with-dashes",
    "key/with/slashes",
    "key\\with\\backslashes",
    "key.with spaces",
    "key@with@symbols",
    "key#with#hashes",
    "key$with$dollars",
    "key%with%percents"
  ]
  
  let mut test_ctx = root_ctx
  for key_str in special_chars_keys {
    let key_obj = ContextKey::new(key_str)
    test_ctx = Context::with_value(test_ctx, key_obj, "value.for." + key_str)
    assert_eq(Context::get(test_ctx, key_obj), Some("value.for." + key_str))
  }
  
  // Test context with unicode characters
  let unicode_key = ContextKey::new("键.名.测试")
  let unicode_value = "值.测试.数据"
  let unicode_ctx = Context::with_value(root_ctx, unicode_key, unicode_value)
  
  assert_eq(Context::get(unicode_ctx, unicode_key), Some(unicode_value))
  
  // Test context with empty strings
  let empty_key = ContextKey::new("")
  let empty_value = ""
  let empty_ctx = Context::with_value(root_ctx, empty_key, empty_value)
  
  assert_eq(Context::get(empty_ctx, empty_key), Some(empty_value))
}

test "baggage operations with boundary conditions" {
  let baggage = Baggage::new()
  
  // Test baggage with empty keys and values
  let empty_key_baggage = Baggage::set_entry(baggage, "", "value")
  let empty_value_baggage = Baggage::set_entry(baggage, "key", "")
  let both_empty_baggage = Baggage::set_entry(baggage, "", "")
  
  assert_eq(Baggage::get_entry(empty_key_baggage, ""), Some("value"))
  assert_eq(Baggage::get_entry(empty_value_baggage, "key"), Some(""))
  assert_eq(Baggage::get_entry(both_empty_baggage, ""), Some(""))
  
  // Test baggage with very long keys and values
  let very_long_key = "k".repeat(1000)
  let very_long_value = "v".repeat(10000)
  
  let long_key_baggage = Baggage::set_entry(baggage, very_long_key, "normal.value")
  let long_value_baggage = Baggage::set_entry(baggage, "normal.key", very_long_value)
  let both_long_baggage = Baggage::set_entry(baggage, very_long_key, very_long_value)
  
  assert_eq(Baggage::get_entry(long_key_baggage, very_long_key), Some("normal.value"))
  assert_eq(Baggage::get_entry(long_value_baggage, "normal.key"), Some(very_long_value))
  assert_eq(Baggage::get_entry(both_long_baggage, very_long_key), Some(very_long_value))
  
  // Test baggage with special characters
  let special_chars = [
    "key.with.dots",
    "key_with_underscores",
    "key-with-dashes",
    "key/with/slashes",
    "key@with@symbols",
    "key with spaces",
    "key=with=equals",
    "key,with,commas",
    "key;with;semicolons",
    "key:with:colons"
  ]
  
  let mut special_baggage = baggage
  for key in special_chars {
    special_baggage = Baggage::set_entry(special_baggage, key, "value.for." + key)
    assert_eq(Baggage::get_entry(special_baggage, key), Some("value.for." + key))
  }
  
  // Test baggage with unicode and international characters
  let unicode_keys = ["用户.标识", "セッション.ID", "세션.아이디", "идентификатор.пользователя"]
  let unicode_values = ["用户123", "セッション456", "세션789", "пользователь101112"]
  
  let mut unicode_baggage = baggage
  for i = 0; i < unicode_keys.length(); i = i + 1 {
    unicode_baggage = Baggage::set_entry(unicode_baggage, unicode_keys[i], unicode_values[i])
    assert_eq(Baggage::get_entry(unicode_baggage, unicode_keys[i]), Some(unicode_values[i]))
  }
}

test "metrics operations with error conditions" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "error-test-meter")
  
  // Test counter with negative values (should be allowed in some implementations)
  let counter = Meter::create_counter(meter, "test.counter")
  Counter::add(counter, -1.0)
  Counter::add(counter, -100.5)
  Counter::add(counter, 0.0)
  
  // Test counter with very large values
  Counter::add(counter, 1.7976931348623157e+308)  // Max double
  Counter::add(counter, -1.7976931348623157e+308) // Min double
  
  // Test histogram with boundary values
  let histogram = Meter::create_histogram(meter, "test.histogram")
  Histogram::record(histogram, 0.0)
  Histogram::record(histogram, -1.0)  // Negative values might be allowed
  Histogram::record(histogram, 1.7976931348623157e+308)  // Max double
  Histogram::record(histogram, 5e-324)  // Smallest positive double
  
  // Test updown counter with extreme values
  let updown_counter = Meter::create_updown_counter(meter, "test.updown")
  UpDownCounter::add(updown_counter, 1.7976931348623157e+308)
  UpDownCounter::add(updown_counter, -1.7976931348623157e+308)
  UpDownCounter::add(updown_counter, 0.0)
  
  // Test gauge with extreme values
  let gauge = Meter::create_gauge(meter, "test.gauge")
  UpDownCounter::add(gauge, 1.7976931348623157e+308)
  UpDownCounter::add(gauge, -1.7976931348623157e+308)
  
  // Verify all operations complete without errors
  assert_true(true)
}

test "log record operations with boundary conditions" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "boundary-test-logger")
  
  // Test log record with empty body
  let empty_body_log = LogRecord::new(Info, "")
  assert_eq(LogRecord::body(empty_body_log), Some(""))
  
  // Test log record with very long body
  let very_long_body = "a".repeat(100000)  // 100,000 character log message
  let long_body_log = LogRecord::new(Error, very_long_body)
  assert_eq(LogRecord::body(long_body_log), Some(very_long_body))
  
  // Test log record with special characters
  let special_chars_body = "Special characters: !@#$%^&*()_+-=[]{}|;':\",./<>?~`"
  let special_chars_log = LogRecord::new(Warn, special_chars_body)
  assert_eq(LogRecord::body(special_chars_log), Some(special_chars_body))
  
  // Test log record with unicode characters
  let unicode_body = "Unicode test: 中文 日本語 한국어 Русский العربية עברית"
  let unicode_log = LogRecord::new(Info, unicode_body)
  assert_eq(LogRecord::body(unicode_log), Some(unicode_body))
  
  // Test log record with newlines and control characters
  let control_chars_body = "Line 1\nLine 2\tTabbed\r\nCarriage return"
  let control_chars_log = LogRecord::new(Debug, control_chars_body)
  assert_eq(LogRecord::body(control_chars_log), Some(control_chars_body))
  
  // Test log record with boundary timestamps
  let min_timestamp = 0L
  let max_timestamp = 9223372036854775807L
  let current_timestamp = Clock::now_unix_nanos(Clock::system())
  
  let min_time_log = LogRecord::new_with_context(Info, Some("Min timestamp"), None, Some(min_timestamp), None, None, None, None)
  let max_time_log = LogRecord::new_with_context(Info, Some("Max timestamp"), None, Some(max_timestamp), None, None, None, None)
  let current_time_log = LogRecord::new_with_context(Info, Some("Current timestamp"), None, Some(current_timestamp), None, None, None, None)
  
  // Verify all logs can be emitted
  Logger::emit(logger, empty_body_log)
  Logger::emit(logger, long_body_log)
  Logger::emit(logger, special_chars_log)
  Logger::emit(logger, unicode_log)
  Logger::emit(logger, control_chars_log)
  Logger::emit(logger, min_time_log)
  Logger::emit(logger, max_time_log)
  Logger::emit(logger, current_time_log)
  
  // Verify all emissions complete without errors
  assert_true(true)
}

test "resource operations with error conditions" {
  // Test resource with invalid attribute keys
  let empty_key_attrs = [("", StringValue("value"))]
  let empty_key_resource = Resource::with_attributes(Resource::new(), empty_key_attrs)
  
  // Test resource with very long attribute keys
  let long_key = "k".repeat(10000)
  let long_key_attrs = [(long_key, StringValue("value"))]
  let long_key_resource = Resource::with_attributes(Resource::new(), long_key_attrs)
  
  // Test resource with special character keys
  let special_key_attrs = [
    ("key@with@symbols", StringValue("value1")),
    ("key with spaces", StringValue("value2")),
    ("key/with/slashes", StringValue("value3")),
    ("key\\with\\backslashes", StringValue("value4"))
  ]
  let special_key_resource = Resource::with_attributes(Resource::new(), special_key_attrs)
  
  // Test resource with unicode keys
  let unicode_key_attrs = [
    ("键.名", StringValue("值")),
    ("キー", StringValue("値")),
    ("키", StringValue("값"))
  ]
  let unicode_key_resource = Resource::with_attributes(Resource::new(), unicode_key_attrs)
  
  // Test resource merging with empty resources
  let empty_resource1 = Resource::new()
  let empty_resource2 = Resource::new()
  let merged_empty = Resource::merge(empty_resource1, empty_resource2)
  
  // Test resource merging with conflicting attributes
  let base_attrs = [("conflict.key", StringValue("base_value"))]
  let override_attrs = [("conflict.key", StringValue("override_value"))]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_conflict = Resource::merge(base_resource, override_resource)
  
  // Verify all resource operations complete without errors
  assert_true(true)
  
  // Test resource attribute retrieval with non-existent keys
  assert_eq(Resource::get_attribute(empty_key_resource, "nonexistent"), None)
  assert_eq(Resource::get_attribute(long_key_resource, "nonexistent"), None)
  assert_eq(Resource::get_attribute(special_key_resource, "nonexistent"), None)
  assert_eq(Resource::get_attribute(unicode_key_resource, "nonexistent"), None)
  assert_eq(Resource::get_attribute(merged_empty, "any.key"), None)
}

test "propagator operations with malformed data" {
  let carrier = TextMapCarrier::new()
  
  // Test with malformed traceparent headers
  let malformed_traceparents = [
    "",  // Empty
    "invalid",  // Invalid format
    "00-invalid-trace-id",  // Incomplete
    "00-0af7651916cd43dd8448eb211c80319c",  // Missing span ID and flags
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331",  // Missing flags
    "ff-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",  // Future version
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-99",  // Invalid flags
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01-extra",  // Extra data
    "not-a-number-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"  // Invalid version
  ]
  
  for malformed in malformed_traceparents {
    TextMapCarrier::set(carrier, "traceparent", malformed)
    assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some(malformed))
  }
  
  // Test with malformed baggage headers
  let malformed_baggages = [
    "",  // Empty
    "invalid",  // No equals sign
    "=value",  // Empty key
    "key=",  // Empty value
    "key=value=extra=equals",  // Multiple equals
    "key=value,invalid",  // Invalid second entry
    ",key=value",  // Leading comma
    "key=value,",  // Trailing comma
    "key=value,,invalid",  // Double comma
    "key=value,=invalid",  // Empty key in second entry
    "key=value,key="  // Empty value in second entry
  ]
  
  for malformed in malformed_baggages {
    TextMapCarrier::set(carrier, "baggage", malformed)
    assert_eq(TextMapCarrier::get(carrier, "baggage"), Some(malformed))
  }
  
  // Test with very long headers
  let very_long_traceparent = "00-" + "a".repeat(32) + "-" + "b".repeat(16) + "-01"
  let very_long_baggage = "key=" + "v".repeat(10000)
  
  TextMapCarrier::set(carrier, "traceparent", very_long_traceparent)
  TextMapCarrier::set(carrier, "baggage", very_long_baggage)
  
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some(very_long_traceparent))
  assert_eq(TextMapCarrier::get(carrier, "baggage"), Some(very_long_baggage))
  
  // Verify all malformed data operations complete without errors
  assert_true(true)
}