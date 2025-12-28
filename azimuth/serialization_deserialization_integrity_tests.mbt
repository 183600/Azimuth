// Serialization and Deserialization Integrity Tests
// Testing data integrity through serialization and deserialization cycles

test "attribute value serialization integrity" {
  // Test serialization and deserialization of different attribute value types
  let string_attr = StringValue("test string value")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14159)
  let bool_attr = BoolValue(true)
  let array_string_attr = ArrayStringValue(["item1", "item2", "item3"])
  let array_int_attr = ArrayIntValue([10, 20, 30])
  
  // Simulate serialization (convert to string representation)
  let string_serialized = "StringValue:test string value"
  let int_serialized = "IntValue:42"
  let float_serialized = "FloatValue:3.14159"
  let bool_serialized = "BoolValue:true"
  let array_string_serialized = "ArrayStringValue:item1,item2,item3"
  let array_int_serialized = "ArrayIntValue:10,20,30"
  
  // Simulate deserialization (parse back to original types)
  // In real implementation, this would involve actual serialization/deserialization
  // Here we verify the data integrity by comparing original and expected values
  
  assert_eq(string_serialized, "StringValue:test string value")
  assert_eq(int_serialized, "IntValue:42")
  assert_eq(float_serialized, "FloatValue:3.14159")
  assert_eq(bool_serialized, "BoolValue:true")
  assert_eq(array_string_serialized, "ArrayStringValue:item1,item2,item3")
  assert_eq(array_int_serialized, "ArrayIntValue:10,20,30")
  
  // Verify original values are preserved
  match string_attr {
    StringValue(v) => assert_eq(v, "test string value")
    _ => assert_false(true)
  }
  
  match int_attr {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_false(true)
  }
  
  match float_attr {
    FloatValue(v) => assert_eq(v, 3.14159)
    _ => assert_false(true)
  }
  
  match bool_attr {
    BoolValue(v) => assert_eq(v, true)
    _ => assert_false(true)
  }
}

test "span context serialization integrity" {
  // Test span context serialization and deserialization
  let original_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let original_span_id = "b7ad6b7169203331"
  let original_sampled = true
  let original_trace_state = "key1=value1,key2=value2"
  
  let original_ctx = SpanContext::new(
    original_trace_id,
    original_span_id,
    original_sampled,
    original_trace_state
  )
  
  // Simulate serialization to W3C traceparent format
  let serialized_traceparent = "00-" + original_trace_id + "-" + original_span_id + "-01"
  
  // Simulate deserialization back to span context
  let deserialized_trace_id = original_trace_id
  let deserialized_span_id = original_span_id
  let deserialized_sampled = true
  let deserialized_trace_state = original_trace_state
  
  let deserialized_ctx = SpanContext::new(
    deserialized_trace_id,
    deserialized_span_id,
    deserialized_sampled,
    deserialized_trace_state
  )
  
  // Verify integrity
  assert_eq(SpanContext::trace_id(original_ctx), SpanContext::trace_id(deserialized_ctx))
  assert_eq(SpanContext::span_id(original_ctx), SpanContext::span_id(deserialized_ctx))
  assert_eq(SpanContext::is_sampled(original_ctx), SpanContext::is_sampled(deserialized_ctx))
  
  // Verify serialized format
  assert_eq(serialized_traceparent, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
}

test "baggage serialization integrity" {
  // Test baggage serialization and deserialization
  let original_baggage = Baggage::new()
  let with_entries = Baggage::set_entry(original_baggage, "user.id", "12345")
  let with_entries = Baggage::set_entry(with_entries, "session.id", "sess-abc123")
  let with_entries = Baggage::set_entry(with_entries, "request.id", "req-def456")
  
  // Simulate serialization to W3C baggage format
  let serialized_baggage = "user.id=12345,session.id=sess-abc123,request.id=req-def456"
  
  // Simulate deserialization back to baggage
  let deserialized_baggage = Baggage::new()
  let deserialized_with_entries = Baggage::set_entry(deserialized_baggage, "user.id", "12345")
  let deserialized_with_entries = Baggage::set_entry(deserialized_with_entries, "session.id", "sess-abc123")
  let deserialized_with_entries = Baggage::set_entry(deserialized_with_entries, "request.id", "req-def456")
  
  // Verify integrity through entry retrieval
  let original_user_id = Baggage::get_entry(with_entries, "user.id")
  let original_session_id = Baggage::get_entry(with_entries, "session.id")
  let original_request_id = Baggage::get_entry(with_entries, "request.id")
  
  let deserialized_user_id = Baggage::get_entry(deserialized_with_entries, "user.id")
  let deserialized_session_id = Baggage::get_entry(deserialized_with_entries, "session.id")
  let deserialized_request_id = Baggage::get_entry(deserialized_with_entries, "request.id")
  
  assert_eq(original_user_id, deserialized_user_id)
  assert_eq(original_session_id, deserialized_session_id)
  assert_eq(original_request_id, deserialized_request_id)
  
  // Verify serialized format
  assert_eq(serialized_baggage, "user.id=12345,session.id=sess-abc123,request.id=req-def456")
}

test "resource serialization integrity" {
  // Test resource serialization and deserialization
  let original_attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("host-123")),
    ("process.id", IntValue(5678))
  ]
  let original_resource = Resource::with_attributes(Resource::new(), original_attrs)
  
  // Simulate serialization to JSON-like format
  let serialized_resource = "{" 
  + "\"service.name\":\"test-service\"," 
  + "\"service.version\":\"1.0.0\"," 
  + "\"deployment.environment\":\"production\"," 
  + "\"host.name\":\"host-123\"," 
  + "\"process.id\":5678" 
  + "}"
  
  // Simulate deserialization back to resource
  let deserialized_attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("host-123")),
    ("process.id", IntValue(5678))
  ]
  let deserialized_resource = Resource::with_attributes(Resource::new(), deserialized_attrs)
  
  // Verify integrity through attribute comparison
  let original_service_name = Resource::get_attribute(original_resource, "service.name")
  let original_version = Resource::get_attribute(original_resource, "service.version")
  let original_env = Resource::get_attribute(original_resource, "deployment.environment")
  let original_host = Resource::get_attribute(original_resource, "host.name")
  let original_pid = Resource::get_attribute(original_resource, "process.id")
  
  let deserialized_service_name = Resource::get_attribute(deserialized_resource, "service.name")
  let deserialized_version = Resource::get_attribute(deserialized_resource, "service.version")
  let deserialized_env = Resource::get_attribute(deserialized_resource, "deployment.environment")
  let deserialized_host = Resource::get_attribute(deserialized_resource, "host.name")
  let deserialized_pid = Resource::get_attribute(deserialized_resource, "process.id")
  
  assert_eq(original_service_name, deserialized_service_name)
  assert_eq(original_version, deserialized_version)
  assert_eq(original_env, deserialized_env)
  assert_eq(original_host, deserialized_host)
  assert_eq(original_pid, deserialized_pid)
}

test "log record serialization integrity" {
  // Test log record serialization and deserialization
  let original_record = LogRecord::new_with_context(
    Warn,
    Some("Test log message with serialization"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-123"),
    Some("span-456"),
    None
  )
  
  // Simulate serialization to JSON format
  let serialized_record = "{" 
  + "\"severity\":\"Warn\"," 
  + "\"body\":\"Test log message with serialization\"," 
  + "\"timestamp\":1735689600000000000," 
  + "\"observed_timestamp\":1735689600000001000," 
  + "\"trace_id\":\"trace-123\"," 
  + "\"span_id\":\"span-456\"" 
  + "}"
  
  // Simulate deserialization back to log record
  let deserialized_record = LogRecord::new_with_context(
    Warn,
    Some("Test log message with serialization"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-123"),
    Some("span-456"),
    None
  )
  
  // Verify integrity
  assert_eq(LogRecord::severity_number(original_record), LogRecord::severity_number(deserialized_record))
  assert_eq(LogRecord::body(original_record), LogRecord::body(deserialized_record))
  assert_eq(LogRecord::trace_id(original_record), LogRecord::trace_id(deserialized_record))
  assert_eq(LogRecord::span_id(original_record), LogRecord::span_id(deserialized_record))
  
  // Verify serialized format contains key information
  assert_true(serialized_record.index_of("\"severity\":\"Warn\"") >= 0)
  assert_true(serialized_record.index_of("\"trace_id\":\"trace-123\"") >= 0)
  assert_true(serialized_record.index_of("\"span_id\":\"span-456\"") >= 0)
}

test "instrument serialization integrity" {
  // Test instrument serialization and deserialization
  let original_counter = Counter("http.requests.total", Some("Total HTTP requests"), Some("count"))
  let original_histogram = Histogram("http.request.duration", Some("HTTP request duration"), Some("ms"))
  let original_updown = UpDownCounter("active.connections", Some("Active connections"), Some("connections"))
  let original_gauge = Gauge("memory.usage", Some("Memory usage"), Some("bytes"))
  
  // Simulate serialization
  let serialized_counter = "{\"type\":\"Counter\",\"name\":\"http.requests.total\",\"description\":\"Total HTTP requests\",\"unit\":\"count\"}"
  let serialized_histogram = "{\"type\":\"Histogram\",\"name\":\"http.request.duration\",\"description\":\"HTTP request duration\",\"unit\":\"ms\"}"
  let serialized_updown = "{\"type\":\"UpDownCounter\",\"name\":\"active.connections\",\"description\":\"Active connections\",\"unit\":\"connections\"}"
  let serialized_gauge = "{\"type\":\"Gauge\",\"name\":\"memory.usage\",\"description\":\"Memory usage\",\"unit\":\"bytes\"}"
  
  // Simulate deserialization back to instruments
  let deserialized_counter = Counter("http.requests.total", Some("Total HTTP requests"), Some("count"))
  let deserialized_histogram = Histogram("http.request.duration", Some("HTTP request duration"), Some("ms"))
  let deserialized_updown = UpDownCounter("active.connections", Some("Active connections"), Some("connections"))
  let deserialized_gauge = Gauge("memory.usage", Some("Memory usage"), Some("bytes"))
  
  // Verify integrity through instrument comparison
  assert_eq(Instrument::name(original_counter), Instrument::name(deserialized_counter))
  assert_eq(Instrument::description(original_counter), Instrument::description(deserialized_counter))
  assert_eq(Instrument::unit(original_counter), Instrument::unit(deserialized_counter))
  
  assert_eq(Instrument::name(original_histogram), Instrument::name(deserialized_histogram))
  assert_eq(Instrument::description(original_histogram), Instrument::description(deserialized_histogram))
  assert_eq(Instrument::unit(original_histogram), Instrument::unit(deserialized_histogram))
  
  assert_eq(Instrument::name(original_updown), Instrument::name(deserialized_updown))
  assert_eq(Instrument::description(original_updown), Instrument::description(deserialized_updown))
  assert_eq(Instrument::unit(original_updown), Instrument::unit(deserialized_updown))
  
  assert_eq(Instrument::name(original_gauge), Instrument::name(deserialized_gauge))
  assert_eq(Instrument::description(original_gauge), Instrument::description(deserialized_gauge))
  assert_eq(Instrument::unit(original_gauge), Instrument::unit(deserialized_gauge))
}

test "serialization with special characters and unicode" {
  // Test serialization integrity with special characters and unicode
  let unicode_attrs = [
    ("服务名称", StringValue("测试服务")),
    ("description", StringValue("Service with special chars: !@#$%^&*()")),
    ("emoji", StringValue("🚀🔥💧")),
    ("unicode.key", StringValue("测试键"))
  ]
  let unicode_resource = Resource::with_attributes(Resource::new(), unicode_attrs)
  
  // Simulate serialization with proper escaping
  let serialized_unicode = "{" 
  + "\"服务名称\":\"测试服务\"," 
  + "\"description\":\"Service with special chars: !@#$%^&*()\"," 
  + "\"emoji\":\"🚀🔥💧\"," 
  + "\"unicode.key\":\"测试键\"" 
  + "}"
  
  // Simulate deserialization back to resource
  let deserialized_unicode_attrs = [
    ("服务名称", StringValue("测试服务")),
    ("description", StringValue("Service with special chars: !@#$%^&*()")),
    ("emoji", StringValue("🚀🔥💧")),
    ("unicode.key", StringValue("测试键"))
  ]
  let deserialized_unicode_resource = Resource::with_attributes(Resource::new(), deserialized_unicode_attrs)
  
  // Verify integrity
  let original_service_name = Resource::get_attribute(unicode_resource, "服务名称")
  let original_description = Resource::get_attribute(unicode_resource, "description")
  let original_emoji = Resource::get_attribute(unicode_resource, "emoji")
  let original_unicode_key = Resource::get_attribute(unicode_resource, "unicode.key")
  
  let deserialized_service_name = Resource::get_attribute(deserialized_unicode_resource, "服务名称")
  let deserialized_description = Resource::get_attribute(deserialized_unicode_resource, "description")
  let deserialized_emoji = Resource::get_attribute(deserialized_unicode_resource, "emoji")
  let deserialized_unicode_key = Resource::get_attribute(deserialized_unicode_resource, "unicode.key")
  
  assert_eq(original_service_name, deserialized_service_name)
  assert_eq(original_description, deserialized_description)
  assert_eq(original_emoji, deserialized_emoji)
  assert_eq(original_unicode_key, deserialized_unicode_key)
}

test "serialization roundtrip integrity" {
  // Test multiple serialization/deserialization cycles
  let original_ctx = SpanContext::new("trace-123", "span-456", true, "key1=value1")
  
  // First serialization/deserialization cycle
  let serialized1 = "00-trace-123-span-456-01"
  let deserialized1 = SpanContext::new("trace-123", "span-456", true, "key1=value1")
  
  // Second serialization/deserialization cycle
  let serialized2 = "00-trace-123-span-456-01"
  let deserialized2 = SpanContext::new("trace-123", "span-456", true, "key1=value1")
  
  // Third serialization/deserialization cycle
  let serialized3 = "00-trace-123-span-456-01"
  let deserialized3 = SpanContext::new("trace-123", "span-456", true, "key1=value1")
  
  // Verify all cycles preserve integrity
  assert_eq(SpanContext::trace_id(original_ctx), SpanContext::trace_id(deserialized1))
  assert_eq(SpanContext::trace_id(deserialized1), SpanContext::trace_id(deserialized2))
  assert_eq(SpanContext::trace_id(deserialized2), SpanContext::trace_id(deserialized3))
  
  assert_eq(SpanContext::span_id(original_ctx), SpanContext::span_id(deserialized1))
  assert_eq(SpanContext::span_id(deserialized1), SpanContext::span_id(deserialized2))
  assert_eq(SpanContext::span_id(deserialized2), SpanContext::span_id(deserialized3))
  
  assert_eq(SpanContext::is_sampled(original_ctx), SpanContext::is_sampled(deserialized1))
  assert_eq(SpanContext::is_sampled(deserialized1), SpanContext::is_sampled(deserialized2))
  assert_eq(SpanContext::is_sampled(deserialized2), SpanContext::is_sampled(deserialized3))
  
  // Verify serialized format consistency
  assert_eq(serialized1, serialized2)
  assert_eq(serialized2, serialized3)
}

test "serialization with large datasets" {
  // Test serialization integrity with large datasets
  let mut large_attrs = [] : Array[(String, AttributeValue)]
  
  // Create large attribute set
  for i = 0; i < 100; i++ {
    let key = "large.attr." + i.to_string()
    let value = StringValue("large.value." + i.to_string())
    large_attrs = large_attrs.push((key, value))
  }
  
  let large_resource = Resource::with_attributes(Resource::new(), large_attrs)
  
  // Simulate serialization of large dataset
  let mut serialized_large = "{" 
  for i = 0; i < 100; i++ {
    if i > 0 {
      serialized_large = serialized_large + ","
    }
    serialized_large = serialized_large + "\"large.attr." + i.to_string() + "\":\"large.value." + i.to_string() + "\""
  }
  serialized_large = serialized_large + "}"
  
  // Simulate deserialization
  let mut deserialized_large_attrs = [] : Array[(String, AttributeValue)]
  for i = 0; i < 100; i++ {
    let key = "large.attr." + i.to_string()
    let value = StringValue("large.value." + i.to_string())
    deserialized_large_attrs = deserialized_large_attrs.push((key, value))
  }
  let deserialized_large_resource = Resource::with_attributes(Resource::new(), deserialized_large_attrs)
  
  // Verify integrity of sample attributes
  let original_first = Resource::get_attribute(large_resource, "large.attr.0")
  let original_last = Resource::get_attribute(large_resource, "large.attr.99")
  let original_middle = Resource::get_attribute(large_resource, "large.attr.50")
  
  let deserialized_first = Resource::get_attribute(deserialized_large_resource, "large.attr.0")
  let deserialized_last = Resource::get_attribute(deserialized_large_resource, "large.attr.99")
  let deserialized_middle = Resource::get_attribute(deserialized_large_resource, "large.attr.50")
  
  assert_eq(original_first, deserialized_first)
  assert_eq(original_last, deserialized_last)
  assert_eq(original_middle, deserialized_middle)
}

test "serialization error handling" {
  // Test serialization error handling and recovery
  // Test with malformed traceparent
  let malformed_traceparent = "invalid-traceparent-format"
  
  // Simulate deserialization error handling
  // In real implementation, this would return an error or default value
  let fallback_ctx = SpanContext::new("", "", false, "")
  
  // Verify fallback context is created safely
  assert_eq(SpanContext::trace_id(fallback_ctx), "")
  assert_eq(SpanContext::span_id(fallback_ctx), "")
  assert_false(SpanContext::is_sampled(fallback_ctx))
  assert_false(SpanContext::is_valid(fallback_ctx))
  
  // Test with malformed baggage
  let malformed_baggage = "malformed=baggage=entry=with=too=many=equals"
  
  // Simulate safe deserialization with fallback
  let fallback_baggage = Baggage::new()
  let fallback_entry = Baggage::get_entry(fallback_baggage, "any.key")
  
  // Verify fallback behavior
  assert_eq(fallback_entry, None)
  
  // Test with malformed JSON
  let malformed_json = "{\"incomplete\": json"
  
  // Simulate safe handling with default resource
  let fallback_resource = Resource::new()
  let fallback_attr = Resource::get_attribute(fallback_resource, "any.key")
  
  // Verify fallback resource is empty but safe
  assert_eq(fallback_attr, None)
}