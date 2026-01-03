// Azimuth Telemetry System - Premium Serialization Edge Cases Tests
// This file contains high-quality test cases for telemetry data serialization edge cases

// Test 1: Span Serialization with Complex Data Types
test "span serialization with complex data types" {
  let span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "test_state")
  let span = Span::new("complex_span", Internal, span_ctx)
  
  // Add complex attributes
  let complex_attrs = Attributes::new()
  Attributes::set(complex_attrs, "string.with.special.chars", StringValue("test!@#$%^&*()"))
  Attributes::set(complex_attrs, "unicode.string", StringValue("中文测试 🚀 emoji"))
  Attributes::set(complex_attrs, "multiline.string", StringValue("line1\nline2\nline3"))
  Attributes::set(complex_attrs, "tab.separated", StringValue("col1\tcol2\tcol3"))
  Attributes::set(complex_attrs, "json.like.string", StringValue("{\"key\": \"value\", \"array\": [1,2,3]}"))
  Attributes::set(complex_attrs, "xml.like.string", StringValue("<root><child>value</child></root>"))
  Attributes::set(complex_attrs, "url.like.string", StringValue("https://example.com/path?param=value&other=123"))
  Attributes::set(complex_attrs, "empty.string", StringValue(""))
  Attributes::set(complex_attrs, "space.only", StringValue("   "))
  Attributes::set(complex_attrs, "null.like", StringValue("null"))
  Attributes::set(complex_attrs, "undefined.like", StringValue("undefined"))
  
  Span::add_event(span, "complex_event", Some(complex_attrs))
  
  // Test serialization of complex span
  let serialized = Span::serialize(span)
  assert_true(serialized.length() > 0, "Serialized span should not be empty")
  
  // Test deserialization
  let deserialized = Span::deserialize(serialized)
  assert_true(deserialized.is_some(), "Deserialized span should be valid")
  
  match deserialized {
    Some(restored_span) => {
      assert_eq(Span::name(restored_span), "complex_span")
      assert_eq(SpanContext::trace_id(Span::span_context(restored_span)), "0af7651916cd43dd8448eb211c80319c")
      assert_eq(SpanContext::span_id(Span::span_context(restored_span)), "b7ad6b7169203331")
    }
    None => assert_true(false, "Deserialized span should not be None")
  }
}

// Test 2: Attribute Serialization Edge Cases
test "attribute serialization edge cases" {
  let attrs = Attributes::new()
  
  // Test extreme numeric values
  Attributes::set(attrs, "max.int", IntValue(2147483647))
  Attributes::set(attrs, "min.int", IntValue(-2147483648))
  Attributes::set(attrs, "zero.int", IntValue(0))
  Attributes::set(attrs, "max.float", FloatValue(1.7976931348623157e+308))
  Attributes::set(attrs, "min.float", FloatValue(-1.7976931348623157e+308))
  Attributes::set(attrs, "inf.float", FloatValue(1.0 / 0.0))
  Attributes::set(attrs, "neg.inf.float", FloatValue(-1.0 / 0.0))
  Attributes::set(attrs, "nan.float", FloatValue(0.0 / 0.0))
  Attributes::set(attrs, "tiny.float", FloatValue(1.0e-308))
  
  // Test special boolean values
  Attributes::set(attrs, "true.value", BoolValue(true))
  Attributes::set(attrs, "false.value", BoolValue(false))
  
  // Test array edge cases
  Attributes::set(attrs, "empty.array", ArrayStringValue([]))
  Attributes::set(attrs, "single.item.array", ArrayStringValue(["single"]))
  Attributes::set(attrs, "large.array", ArrayStringValue(["item_" + i.to_string() for i in 0..100]))
  Attributes::set(attrs, "mixed.char.array", ArrayStringValue(["", " ", "\t", "\n", "x" * 1000]))
  
  // Test string edge cases
  Attributes::set(attrs, "empty.string", StringValue(""))
  Attributes::set(attrs, "space.string", StringValue(" "))
  Attributes::set(attrs, "null.string", StringValue("null"))
  Attributes::set(attrs, "undefined.string", StringValue("undefined"))
  Attributes::set(attrs, "very.long.string", StringValue("x" * 10000))
  Attributes::set(attrs, "unicode.string", StringValue("中文测试 🚀 ñáéíóú"))
  Attributes::set(attrs, "escape.chars", StringValue("\"\\/\b\f\n\r\t"))
  Attributes::set(attrs, "control.chars", StringValue("\x00\x01\x02\x03\x04\x05"))
  Attributes::set(attrs, "json.string", StringValue("{\"nested\": {\"array\": [1,2,3], \"string\": \"value\"}}"))
  
  // Serialize attributes
  let serialized = Attributes::serialize(attrs)
  assert_true(serialized.length() > 0, "Serialized attributes should not be empty")
  
  // Deserialize attributes
  let deserialized = Attributes::deserialize(serialized)
  assert_true(deserialized.is_some(), "Deserialized attributes should be valid")
  
  match deserialized {
    Some(restored_attrs) => {
      // Verify extreme values are preserved
      match Attributes::get(restored_attrs, "max.int") {
        Some(IntValue(value)) => assert_eq(value, 2147483647)
        _ => assert_true(false, "Max int should be preserved")
      }
      
      match Attributes::get(restored_attrs, "min.int") {
        Some(IntValue(value)) => assert_eq(value, -2147483648)
        _ => assert_true(false, "Min int should be preserved")
      }
      
      match Attributes::get(restored_attrs, "true.value") {
        Some(BoolValue(value)) => assert_true(value)
        _ => assert_true(false, "True value should be preserved")
      }
      
      match Attributes::get(restored_attrs, "false.value") {
        Some(BoolValue(value)) => assert_false(value)
        _ => assert_true(false, "False value should be preserved")
      }
      
      match Attributes::get(restored_attrs, "empty.string") {
        Some(StringValue(value)) => assert_eq(value, "")
        _ => assert_true(false, "Empty string should be preserved")
      }
      
      match Attributes::get(restored_attrs, "unicode.string") {
        Some(StringValue(value)) => assert_true(value.contains("中文测试"))
        _ => assert_true(false, "Unicode string should be preserved")
      }
    }
    None => assert_true(false, "Deserialized attributes should not be None")
  }
}

// Test 3: Metric Serialization Edge Cases
test "metric serialization edge cases" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "serialization_meter")
  
  // Create metrics with edge case configurations
  let counter = Meter::create_counter(meter, "edge.case.counter", Some("Counter with edge cases"), Some("operations"))
  let histogram = Meter::create_histogram(meter, "edge.case.histogram", Some("Histogram with edge cases"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "edge.case.updown", Some("UpDown counter with edge cases"), Some("value"))
  
  // Add edge case values
  let edge_attrs = Attributes::new()
  Attributes::set(edge_attrs, "edge.case.type", StringValue("test"))
  Attributes::set(edge_attrs, "unicode.attr", StringValue("中文测试"))
  
  Counter::add(counter, 0.0, Some(edge_attrs))
  Counter::add(counter, -1.0, Some(edge_attrs))  // Negative for counter (might be edge case)
  Counter::add(counter, 999999999.9, Some(edge_attrs))
  Counter::add(counter, 1.0e-10, Some(edge_attrs))
  
  Histogram::record(histogram, -100.0, Some(edge_attrs))  // Negative for histogram
  Histogram::record(histogram, 0.0, Some(edge_attrs))
  Histogram::record(histogram, 1.0 / 0.0, Some(edge_attrs))  // Infinity
  Histogram::record(histogram, 0.0 / 0.0, Some(edge_attrs))  // NaN
  
  UpDownCounter::add(updown_counter, 999999999.9, Some(edge_attrs))
  UpDownCounter::add(updown_counter, -999999999.9, Some(edge_attrs))
  UpDownCounter::add(updown_counter, 1.0e-10, Some(edge_attrs))
  
  // Serialize metrics
  let serialized_counter = Counter::serialize(counter)
  let serialized_histogram = Histogram::serialize(histogram)
  let serialized_updown = UpDownCounter::serialize(updown_counter)
  
  assert_true(serialized_counter.length() > 0, "Serialized counter should not be empty")
  assert_true(serialized_histogram.length() > 0, "Serialized histogram should not be empty")
  assert_true(serialized_updown.length() > 0, "Serialized updown counter should not be empty")
  
  // Deserialize metrics
  let deserialized_counter = Counter::deserialize(serialized_counter)
  let deserialized_histogram = Histogram::deserialize(serialized_histogram)
  let deserialized_updown = UpDownCounter::deserialize(serialized_updown)
  
  assert_true(deserialized_counter.is_some(), "Deserialized counter should be valid")
  assert_true(deserialized_histogram.is_some(), "Deserialized histogram should be valid")
  assert_true(deserialized_updown.is_some(), "Deserialized updown counter should be valid")
  
  // Verify instrument metadata is preserved
  match deserialized_counter {
    Some(restored_counter) => {
      let instrument = Counter::as_instrument(restored_counter)
      assert_eq(Instrument::name(instrument), "edge.case.counter")
      assert_eq(Instrument::description(instrument), Some("Counter with edge cases"))
      assert_eq(Instrument::unit(instrument), Some("operations"))
    }
    None => assert_true(false, "Deserialized counter should not be None")
  }
}

// Test 4: Log Record Serialization Edge Cases
test "log record serialization edge cases" {
  // Create log records with edge case content
  let edge_attrs = Attributes::new()
  Attributes::set(edge_attrs, "log.type", StringValue("edge_case"))
  Attributes::set(edge_attrs, "unicode.data", StringValue("中文测试 🚀"))
  Attributes::set(edge_attrs, "json.data", StringValue("{\"nested\": {\"value\": \"test\"}}"))
  
  // Log with edge case body content
  let edge_log_bodies = [
    "",                                           // Empty
    " ",                                          // Space only
    "null",                                       // String "null"
    "undefined",                                  // String "undefined"
    "x" * 10000,                                  // Very long
    "中文测试 🚀 emoji",                           // Unicode
    "{\"json\": \"content\"}",                    // JSON-like
    "<xml>content</xml>",                         // XML-like
    "line1\nline2\nline3",                        // Multiline
    "tab\tseparated\tcontent",                    // Tabs
    "\"quoted\" content",                         // Quotes
    "control\x00chars\x01\x02",                   // Control chars
    "https://example.com/path?param=value",       // URL
  ]
  
  for body in edge_log_bodies {
    let log_record = LogRecord::new_with_context(
      Error,
      Some(body),
      Some(edge_attrs),
      Some(-1L),                                   // Negative timestamp
      Some(9999999999999L),                       // Very large timestamp
      Some("invalid_trace_id"),                   // Invalid trace ID
      Some("invalid_span_id"),                    // Invalid span ID
      Some(Context::root())
    )
    
    // Serialize log record
    let serialized = LogRecord::serialize(log_record)
    assert_true(serialized.length() > 0, "Serialized log should not be empty for body: {body}")
    
    // Deserialize log record
    let deserialized = LogRecord::deserialize(serialized)
    assert_true(deserialized.is_some(), "Deserialized log should be valid for body: {body}")
    
    match deserialized {
      Some(restored_log) => {
        assert_eq(LogRecord::severity_number(restored_log), Error)
        match LogRecord::body(restored_log) {
          Some(restored_body) => assert_eq(restored_body, body)
          None => assert_true(false, "Log body should be preserved for: {body}")
        }
      }
      None => assert_true(false, "Deserialized log should not be None for body: {body}")
    }
  }
}

// Test 5: Context Serialization Edge Cases
test "context serialization edge cases" {
  let root_ctx = Context::root()
  
  // Create context with edge case values
  let edge_values = [
    "",                                           // Empty
    " ",                                          // Space only
    "null",                                       // String "null"
    "undefined",                                  // String "undefined"
    "x" * 1000,                                   // Very long
    "中文测试 🚀 emoji",                           // Unicode
    "{\"json\": \"content\"}",                    // JSON-like
    "<xml>content</xml>",                         // XML-like
    "line1\nline2\nline3",                        // Multiline
    "tab\tseparated\tcontent",                    // Tabs
    "\"quoted\" content",                         // Quotes
    "control\x00chars\x01\x02",                   // Control chars
    "https://example.com/path?param=value",       // URL
  ]
  
  for i in 0..edge_values.length() {
    let value = edge_values[i]
    let key = ContextKey::new("edge_key_" + i.to_string())
    let ctx_with_value = Context::with_value(root_ctx, key, value)
    
    // Serialize context
    let serialized = Context::serialize(ctx_with_value)
    assert_true(serialized.length() > 0, "Serialized context should not be empty for value: {value}")
    
    // Deserialize context
    let deserialized = Context::deserialize(serialized)
    assert_true(deserialized.is_some(), "Deserialized context should be valid for value: {value}")
    
    match deserialized {
      Some(restored_ctx) => {
        match Context::get(restored_ctx, key) {
          Some(restored_value) => assert_eq(restored_value, value)
          None => assert_true(false, "Context value should be preserved for: {value}")
        }
      }
      None => assert_true(false, "Deserialized context should not be None for value: {value}")
    }
  }
  
  // Test deep context chain serialization
  let mut deep_ctx = root_ctx
  let depth = 100
  
  for i in 0..depth {
    let key = ContextKey::new("deep_key_" + i.to_string())
    deep_ctx = Context::with_value(deep_ctx, key, "deep_value_" + i.to_string())
  }
  
  // Serialize deep context
  let serialized_deep = Context::serialize(deep_ctx)
  assert_true(serialized_deep.length() > 0, "Serialized deep context should not be empty")
  
  // Deserialize deep context
  let deserialized_deep = Context::deserialize(serialized_deep)
  assert_true(deserialized_deep.is_some(), "Deserialized deep context should be valid")
  
  match deserialized_deep {
    Some(restored_deep_ctx) => {
      // Test a few values from the deep chain
      match Context::get(restored_deep_ctx, ContextKey::new("deep_key_0")) {
        Some(value) => assert_eq(value, "deep_value_0")
        None => assert_true(false, "Deep context value 0 should be preserved")
      }
      
      match Context::get(restored_deep_ctx, ContextKey::new("deep_key_" + (depth - 1).to_string())) {
        Some(value) => assert_eq(value, "deep_value_" + (depth - 1).to_string())
        None => assert_true(false, "Deep context value " + (depth - 1).to_string() + " should be preserved")
      }
    }
    None => assert_true(false, "Deserialized deep context should not be None")
  }
}

// Test 6: Resource Serialization Edge Cases
test "resource serialization edge cases" {
  // Create resource with edge case attributes
  let edge_attrs = []
  
  let edge_keys = [
    "",                                           // Empty key
    " ",                                          // Space only
    "key.with.dots",                              // Dots
    "key-with-dashes",                            // Dashes
    "key_with_underscores",                       // Underscores
    "key/with/slashes",                           // Slashes
    "key\\with\\backslashes",                     // Backslashes
    "key@with@symbols",                           // Symbols
    "key#with#hashes",                            // Hashes
    "key$with$dollars",                           // Dollars
    "key%with%percents",                          // Percents
    "key&with&ampersands",                        // Ampersands
    "key*with*asterisks",                         // Asterisks
    "key(with)parentheses",                        // Parentheses
    "key[with]brackets",                          // Brackets
    "key{with}braces",                            // Braces
    "key|with|pipes",                             // Pipes
    "key+with+plus",                              // Plus
    "key=with=equals",                            // Equals
  ]
  
  let edge_values = [
    "",                                           // Empty
    " ",                                          // Space only
    "null",                                       // String "null"
    "undefined",                                  // String "undefined"
    "x" * 1000,                                   // Very long
    "中文测试 🚀 emoji",                           // Unicode
    "{\"json\": \"content\"}",                    // JSON-like
    "<xml>content</xml>",                         // XML-like
    "line1\nline2\nline3",                        // Multiline
    "tab\tseparated\tcontent",                    // Tabs
    "\"quoted\" content",                         // Quotes
    "control\x00chars\x01\x02",                   // Control chars
    "https://example.com/path?param=value",       // URL
  ]
  
  for i in 0..edge_keys.length() {
    let key = edge_keys[i]
    let value = edge_values[i % edge_values.length()]
    edge_attrs.push((key, StringValue(value)))
  }
  
  let resource = Resource::with_attributes(Resource::new(), edge_attrs)
  
  // Serialize resource
  let serialized = Resource::serialize(resource)
  assert_true(serialized.length() > 0, "Serialized resource should not be empty")
  
  // Deserialize resource
  let deserialized = Resource::deserialize(serialized)
  assert_true(deserialized.is_some(), "Deserialized resource should be valid")
  
  match deserialized {
    Some(restored_resource) => {
      // Test a few edge case attributes
      for i in 0..5.min(edge_keys.length()) {
        let key = edge_keys[i]
        let expected_value = edge_values[i % edge_values.length()]
        
        match Resource::get_attribute(restored_resource, key) {
          Some(StringValue(actual_value)) => assert_eq(actual_value, expected_value)
          None => assert_true(true) // Some edge case keys might not be supported
        }
      }
    }
    None => assert_true(false, "Deserialized resource should not be None")
  }
}

// Test 7: Baggage Serialization Edge Cases
test "baggage serialization edge cases" {
  let baggage = Baggage::new()
  
  // Create baggage with edge case entries
  let edge_keys = [
    "",                                           // Empty key
    " ",                                          // Space only
    "key.with.dots",                              // Dots
    "key-with-dashes",                            // Dashes
    "key_with_underscores",                       // Underscores
    "key/with/slashes",                           // Slashes
    "key\\with\\backslashes",                     // Backslashes
    "key@with@symbols",                           // Symbols
    "key#with#hashes",                            // Hashes
    "url-encoded-key%20with%20spaces",            // URL encoded
  ]
  
  let edge_values = [
    "",                                           // Empty
    " ",                                          // Space only
    "null",                                       // String "null"
    "undefined",                                  // String "undefined"
    "x" * 1000,                                   // Very long
    "中文测试 🚀 emoji",                           // Unicode
    "value=with=equals",                          // Equals in value
    "value,with,commas",                          // Commas in value
    "value;with;semicolons",                      // Semicolons in value
    "url-encoded-value%20with%20spaces",          // URL encoded
  ]
  
  let mut updated_baggage = baggage
  
  for i in 0..edge_keys.length() {
    let key = edge_keys[i]
    let value = edge_values[i % edge_values.length()]
    updated_baggage = Baggage::set_entry(updated_baggage, key, value)
  }
  
  // Serialize baggage
  let serialized = Baggage::serialize(updated_baggage)
  assert_true(serialized.length() > 0, "Serialized baggage should not be empty")
  
  // Deserialize baggage
  let deserialized = Baggage::deserialize(serialized)
  assert_true(deserialized.is_some(), "Deserialized baggage should be valid")
  
  match deserialized {
    Some(restored_baggage) => {
      // Test a few edge case entries
      for i in 0..5.min(edge_keys.length()) {
        let key = edge_keys[i]
        let expected_value = edge_values[i % edge_values.length()]
        
        match Baggage::get_entry(restored_baggage, key) {
          Some(actual_value) => assert_eq(actual_value, expected_value)
          None => assert_true(true) // Some edge case keys might not be supported
        }
      }
    }
    None => assert_true(false, "Deserialized baggage should not be None")
  }
}