// Comprehensive Edge Case Tests for Azimuth Telemetry System
// This file contains tests for boundary conditions and edge cases

test "empty string attribute values" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "empty.string", StringValue(""))
  Attributes::set(attrs, "normal.string", StringValue("normal"))
  
  let empty_value = Attributes::get(attrs, "empty.string")
  let normal_value = Attributes::get(attrs, "normal.string")
  
  match empty_value {
    Some(StringValue(s)) => assert_eq(s, "")
    _ => assert_true(false)
  }
  
  match normal_value {
    Some(StringValue(s)) => assert_eq(s, "normal")
    _ => assert_true(false)
  }
}

test "maximum numeric values" {
  let attrs = Attributes::new()
  let max_int = 2147483647  // Max 32-bit signed integer
  let max_float = 1.7976931348623157e+308  // Max double
  
  Attributes::set(attrs, "max.int", IntValue(max_int))
  Attributes::set(attrs, "max.float", FloatValue(max_float))
  
  let int_result = Attributes::get(attrs, "max.int")
  let float_result = Attributes::get(attrs, "max.float")
  
  match int_result {
    Some(IntValue(v)) => assert_eq(v, max_int)
    _ => assert_true(false)
  }
  
  match float_result {
    Some(FloatValue(v)) => assert_true(v > 1.0e+307)  // Just check it's a large number
    _ => assert_true(false)
  }
}

test "minimum numeric values" {
  let attrs = Attributes::new()
  let min_int = -2147483648  // Min 32-bit signed integer
  let min_float = -1.7976931348623157e+308  // Min double
  
  Attributes::set(attrs, "min.int", IntValue(min_int))
  Attributes::set(attrs, "min.float", FloatValue(min_float))
  
  let int_result = Attributes::get(attrs, "min.int")
  let float_result = Attributes::get(attrs, "min.float")
  
  match int_result {
    Some(IntValue(v)) => assert_eq(v, min_int)
    _ => assert_true(false)
  }
  
  match float_result {
    Some(FloatValue(v)) => assert_true(v < -1.0e+307)  // Just check it's a large negative number
    _ => assert_true(false)
  }
}

test "zero values handling" {
  let attrs = Attributes::new()
  
  Attributes::set(attrs, "zero.int", IntValue(0))
  Attributes::set(attrs, "zero.float", FloatValue(0.0))
  Attributes::set(attrs, "zero.bool", BoolValue(false))
  
  let int_result = Attributes::get(attrs, "zero.int")
  let float_result = Attributes::get(attrs, "zero.float")
  let bool_result = Attributes::get(attrs, "zero.bool")
  
  match int_result {
    Some(IntValue(v)) => assert_eq(v, 0)
    _ => assert_true(false)
  }
  
  match float_result {
    Some(FloatValue(v)) => assert_true(v == 0.0)
    _ => assert_true(false)
  }
  
  match bool_result {
    Some(BoolValue(v)) => assert_false(v)
    _ => assert_true(false)
  }
}

test "array edge cases" {
  let attrs = Attributes::new()
  
  // Empty arrays
  Attributes::set(attrs, "empty.string.array", ArrayStringValue([]))
  Attributes::set(attrs, "empty.int.array", ArrayIntValue([]))
  
  // Single element arrays
  Attributes::set(attrs, "single.string.array", ArrayStringValue(["single"]))
  Attributes::set(attrs, "single.int.array", ArrayIntValue([1]))
  
  let empty_str_array = Attributes::get(attrs, "empty.string.array")
  let single_str_array = Attributes::get(attrs, "single.string.array")
  
  match empty_str_array {
    Some(ArrayStringValue(arr)) => assert_eq(arr.length(), 0)
    _ => assert_true(false)
  }
  
  match single_str_array {
    Some(ArrayStringValue(arr)) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], "single")
    }
    _ => assert_true(false)
  }
}

test "context with nested keys" {
  let ctx = Context::root()
  let nested_key = ContextKey::new("very.deeply.nested.key.hierarchy")
  let ctx_with_nested = Context::with_value(ctx, nested_key, "nested_value")
  
  let retrieved = Context::get(ctx_with_nested, nested_key)
  assert_eq(retrieved, Some("nested_value"))
}

test "span context with edge case IDs" {
  // Test with empty trace ID
  let empty_trace = SpanContext::new("", "span123", true, "")
  assert_false(SpanContext::is_valid(empty_trace))
  
  // Test with empty span ID
  let empty_span = SpanContext::new("trace123", "", true, "")
  assert_false(SpanContext::is_valid(empty_span))
  
  // Test with both empty
  let both_empty = SpanContext::new("", "", true, "")
  assert_false(SpanContext::is_valid(both_empty))
  
  // Test with valid IDs
  let valid = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  assert_true(SpanContext::is_valid(valid))
}

test "extremely long strings" {
  let attrs = Attributes::new()
  let long_string = "a" * 10000  // 10,000 character string
  
  Attributes::set(attrs, "long.string", StringValue(long_string))
  
  let result = Attributes::get(attrs, "long.string")
  match result {
    Some(StringValue(s)) => assert_eq(s.length(), 10000)
    _ => assert_true(false)
  }
}

test "special characters in attribute values" {
  let attrs = Attributes::new()
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let unicode_chars = "测试中文字符🚀emoji"
  let newlines = "line1\nline2\r\nline3"
  
  Attributes::set(attrs, "special.chars", StringValue(special_chars))
  Attributes::set(attrs, "unicode.chars", StringValue(unicode_chars))
  Attributes::set(attrs, "newlines", StringValue(newlines))
  
  let special_result = Attributes::get(attrs, "special.chars")
  let unicode_result = Attributes::get(attrs, "unicode.chars")
  let newline_result = Attributes::get(attrs, "newlines")
  
  match special_result {
    Some(StringValue(s)) => assert_eq(s, special_chars)
    _ => assert_true(false)
  }
  
  match unicode_result {
    Some(StringValue(s)) => assert_eq(s, unicode_chars)
    _ => assert_true(false)
  }
  
  match newline_result {
    Some(StringValue(s)) => assert_eq(s, newlines)
    _ => assert_true(false)
  }
}

test "baggage with special key-value pairs" {
  let baggage = Baggage::new()
  
  // Test with empty key
  let baggage_empty_key = Baggage::set_entry(baggage, "", "value")
  
  // Test with empty value
  let baggage_empty_value = Baggage::set_entry(baggage, "key", "")
  
  // Test with special characters
  let baggage_special = Baggage::set_entry(baggage, "special=key", "special=value")
  
  // Test with unicode
  let baggage_unicode = Baggage::set_entry(baggage, "测试键", "测试值")
  
  // Verify retrieval works
  let empty_value_result = Baggage::get_entry(baggage_empty_value, "key")
  let special_result = Baggage::get_entry(baggage_special, "special=key")
  let unicode_result = Baggage::get_entry(baggage_unicode, "测试键")
  
  assert_eq(empty_value_result, Some(""))
  assert_eq(special_result, Some("special=value"))
  assert_eq(unicode_result, Some("测试值"))
}

test "resource with many attributes" {
  let resource = Resource::new()
  
  // Create many attributes
  let mut attrs = []
  for i in 0..=100 {
    attrs = attrs.push(("attr." + i.to_string(), StringValue("value." + i.to_string())))
  }
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Test retrieving some attributes
  let first_attr = Resource::get_attribute(resource_with_attrs, "attr.0")
  let middle_attr = Resource::get_attribute(resource_with_attrs, "attr.50")
  let last_attr = Resource::get_attribute(resource_with_attrs, "attr.100")
  
  match first_attr {
    Some(StringValue(s)) => assert_eq(s, "value.0")
    _ => assert_true(false)
  }
  
  match middle_attr {
    Some(StringValue(s)) => assert_eq(s, "value.50")
    _ => assert_true(false)
  }
  
  match last_attr {
    Some(StringValue(s)) => assert_eq(s, "value.100")
    _ => assert_true(false)
  }
}