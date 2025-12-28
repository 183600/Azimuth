// Attribute Operations Test Suite for Azimuth Telemetry System
// Testing comprehensive attribute value operations and type conversions

test "attribute value string operations" {
  let string_attr = StringValue("test_value")
  assert_eq(string_attr, StringValue("test_value"))
  assert_true(string_attr != StringValue("different_value"))
}

test "attribute value numeric operations" {
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14)
  
  assert_eq(int_attr, IntValue(42))
  assert_eq(float_attr, FloatValue(3.14))
  assert_true(int_attr != IntValue(43))
  assert_true(float_attr != FloatValue(2.71))
}

test "attribute value boolean operations" {
  let true_attr = BoolValue(true)
  let false_attr = BoolValue(false)
  
  assert_eq(true_attr, BoolValue(true))
  assert_eq(false_attr, BoolValue(false))
  assert_true(true_attr != false_attr)
}

test "attribute value array operations" {
  let string_array_attr = ArrayStringValue(["item1", "item2", "item3"])
  let int_array_attr = ArrayIntValue([1, 2, 3, 4, 5])
  
  assert_eq(string_array_attr, ArrayStringValue(["item1", "item2", "item3"]))
  assert_eq(int_array_attr, ArrayIntValue([1, 2, 3, 4, 5]))
  assert_true(string_array_attr != ArrayStringValue(["different"]))
}

test "attributes collection operations" {
  let attrs = Attributes::new()
  
  // Test setting and getting string attributes
  Attributes::set(attrs, "string.key", StringValue("test_string"))
  let string_result = Attributes::get(attrs, "string.key")
  assert_eq(string_result, Some(StringValue("test_string")))
  
  // Test setting and getting int attributes
  Attributes::set(attrs, "int.key", IntValue(123))
  let int_result = Attributes::get(attrs, "int.key")
  assert_eq(int_result, Some(IntValue(123)))
  
  // Test missing key
  let missing_result = Attributes::get(attrs, "missing.key")
  assert_eq(missing_result, None)
}

test "attributes type consistency" {
  let attrs = Attributes::new()
  
  // Set different types for the same key pattern
  Attributes::set(attrs, "test.string", StringValue("string_value"))
  Attributes::set(attrs, "test.int", IntValue(456))
  Attributes::set(attrs, "test.float", FloatValue(7.89))
  Attributes::set(attrs, "test.bool", BoolValue(true))
  
  // Verify each type is correctly retrieved
  let string_val = Attributes::get(attrs, "test.string")
  let int_val = Attributes::get(attrs, "test.int")
  let float_val = Attributes::get(attrs, "test.float")
  let bool_val = Attributes::get(attrs, "test.bool")
  
  assert_eq(string_val, Some(StringValue("string_value")))
  assert_eq(int_val, Some(IntValue(456)))
  assert_eq(float_val, Some(FloatValue(7.89)))
  assert_eq(bool_val, Some(BoolValue(true)))
}

test "attributes edge cases" {
  let attrs = Attributes::new()
  
  // Test empty string
  Attributes::set(attrs, "empty.string", StringValue(""))
  let empty_result = Attributes::get(attrs, "empty.string")
  assert_eq(empty_result, Some(StringValue("")))
  
  // Test zero values
  Attributes::set(attrs, "zero.int", IntValue(0))
  let zero_int_result = Attributes::get(attrs, "zero.int")
  assert_eq(zero_int_result, Some(IntValue(0)))
  
  Attributes::set(attrs, "zero.float", FloatValue(0.0))
  let zero_float_result = Attributes::get(attrs, "zero.float")
  assert_eq(zero_float_result, Some(FloatValue(0.0)))
}

test "attributes complex key names" {
  let attrs = Attributes::new()
  
  // Test complex key names with special characters
  let complex_keys = [
    "service.name",
    "http.method",
    "user.id",
    "error.type",
    "db.statement",
    "messaging.system",
    "cloud.provider",
    "k8s.pod.name"
  ]
  
  for key in complex_keys {
    Attributes::set(attrs, key, StringValue("value_for_" + key))
    let result = Attributes::get(attrs, key)
    assert_eq(result, Some(StringValue("value_for_" + key)))
  }
}

test "attributes array value operations" {
  let attrs = Attributes::new()
  
  // Test string array
  let string_array = ["error", "warning", "info"]
  Attributes::set(attrs, "log.levels", ArrayStringValue(string_array))
  let string_array_result = Attributes::get(attrs, "log.levels")
  assert_eq(string_array_result, Some(ArrayStringValue(string_array)))
  
  // Test int array
  let int_array = [200, 404, 500]
  Attributes::set(attrs, "http.status.codes", ArrayIntValue(int_array))
  let int_array_result = Attributes::get(attrs, "http.status.codes")
  assert_eq(int_array_result, Some(ArrayIntValue(int_array)))
}

test "attributes performance operations" {
  let attrs = Attributes::new()
  
  // Test multiple operations
  for i in 0..100 {
    let key = "performance.key." + i.to_string()
    Attributes::set(attrs, key, IntValue(i))
  }
  
  // Verify a few values
  let first_val = Attributes::get(attrs, "performance.key.0")
  let middle_val = Attributes::get(attrs, "performance.key.50")
  let last_val = Attributes::get(attrs, "performance.key.99")
  
  assert_eq(first_val, Some(IntValue(0)))
  assert_eq(middle_val, Some(IntValue(50)))
  assert_eq(last_val, Some(IntValue(99)))
}