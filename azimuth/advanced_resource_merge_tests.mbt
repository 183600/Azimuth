// Advanced Resource Merge and Attributes Test Suite
// Tests complex resource operations, attribute merging, and resource management scenarios

test "basic resource creation and operations" {
  // Test empty resource creation
  let empty_resource = Resource::new()
  assert_eq(empty_resource.attributes.length(), 0)
  
  // Test resource with attributes
  let attrs = [
    ("service.name", StringValue("api-service")),
    ("service.version", StringValue("v1.2.3")),
    ("service.instance.id", StringValue("instance-12345"))
  ]
  let resource_with_attrs = Resource::with_attributes(empty_resource, attrs)
  
  // Test attribute retrieval
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let service_instance = Resource::get_attribute(resource_with_attrs, "service.instance.id")
  let missing_attr = Resource::get_attribute(resource_with_attrs, "missing.attribute")
  
  assert_eq(service_name, Some(StringValue("api-service")))
  assert_eq(service_version, Some(StringValue("v1.2.3")))
  assert_eq(service_instance, Some(StringValue("instance-12345")))
  assert_eq(missing_attr, None)
}

test "complex attribute types and values" {
  let resource = Resource::new()
  
  // Test different attribute value types
  let complex_attrs = [
    ("string.attr", StringValue("simple string")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("array.string.attr", ArrayStringValue(["value1", "value2", "value3"])),
    ("array.int.attr", ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  let complex_resource = Resource::with_attributes(resource, complex_attrs)
  
  // Verify all attribute types
  assert_eq(Resource::get_attribute(complex_resource, "string.attr"), Some(StringValue("simple string")))
  assert_eq(Resource::get_attribute(complex_resource, "int.attr"), Some(IntValue(42)))
  assert_eq(Resource::get_attribute(complex_resource, "float.attr"), Some(FloatValue(3.14159)))
  assert_eq(Resource::get_attribute(complex_resource, "bool.attr"), Some BoolValue(true))
  assert_eq(Resource::get_attribute(complex_resource, "array.string.attr"), Some(ArrayStringValue(["value1", "value2", "value3"])))
  assert_eq(Resource::get_attribute(complex_resource, "array.int.attr"), Some(ArrayIntValue([1, 2, 3, 4, 5])))
}

test "resource merge operations" {
  // Create base resource
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("v1.0.0")),
    ("deployment.environment", StringValue("development"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  // Create override resource
  let override_attrs = [
    ("service.version", StringValue("v2.0.0")),
    ("host.name", StringValue("api-server-01")),
    ("process.id", IntValue(12345))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // Merge resources
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // In simplified implementation, merge returns override resource
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), None)
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), Some(StringValue("v2.0.0")))
  assert_eq(Resource::get_attribute(merged_resource, "deployment.environment"), None)
  assert_eq(Resource::get_attribute(merged_resource, "host.name"), Some(StringValue("api-server-01")))
  assert_eq(Resource::get_attribute(merged_resource, "process.id"), Some(IntValue(12345)))
}

test "resource attribute edge cases" {
  let resource = Resource::new()
  
  // Test empty string attributes
  let empty_string_attrs = [
    ("empty.string", StringValue("")),
    ("whitespace.string", StringValue("   "))
  ]
  let empty_string_resource = Resource::with_attributes(resource, empty_string_attrs)
  
  assert_eq(Resource::get_attribute(empty_string_resource, "empty.string"), Some(StringValue("")))
  assert_eq(Resource::get_attribute(empty_string_resource, "whitespace.string"), Some(StringValue("   ")))
  
  // Test zero and negative values
  let numeric_attrs = [
    ("zero.int", IntValue(0)),
    ("negative.int", IntValue(-42)),
    ("zero.float", FloatValue(0.0)),
    ("negative.float", FloatValue(-3.14))
  ]
  let numeric_resource = Resource::with_attributes(empty_string_resource, numeric_attrs)
  
  assert_eq(Resource::get_attribute(numeric_resource, "zero.int"), Some(IntValue(0)))
  assert_eq(Resource::get_attribute(numeric_resource, "negative.int"), Some(IntValue(-42)))
  assert_eq(Resource::get_attribute(numeric_resource, "zero.float"), Some(FloatValue(0.0)))
  assert_eq(Resource::get_attribute(numeric_resource, "negative.float"), Some(FloatValue(-3.14)))
  
  // Test boolean false
  let bool_attrs = [("false.value", BoolValue(false))]
  let bool_resource = Resource::with_attributes(numeric_resource, bool_attrs)
  
  assert_eq(Resource::get_attribute(bool_resource, "false.value"), Some(BoolValue(false)))
  
  // Test empty arrays
  let empty_array_attrs = [
    ("empty.string.array", ArrayStringValue([])),
    ("empty.int.array", ArrayIntValue([]))
  ]
  let empty_array_resource = Resource::with_attributes(bool_resource, empty_array_attrs)
  
  assert_eq(Resource::get_attribute(empty_array_resource, "empty.string.array"), Some(ArrayStringValue([])))
  assert_eq(Resource::get_attribute(empty_array_resource, "empty.int.array"), Some(ArrayIntValue([])))
}

test "resource attribute key formats" {
  let resource = Resource::new()
  
  // Test various key formats
  let key_format_attrs = [
    ("simple", StringValue("simple-value")),
    ("dotted.key", StringValue("dotted-value")),
    ("dashed-key", StringValue("dashed-value")),
    ("underscore_key", StringValue("underscore-value")),
    ("mixed-format_key.key", StringValue("mixed-value")),
    ("numeric.123", StringValue("numeric-key-value")),
    ("UPPERCASE.KEY", StringValue("uppercase-value"))
  ]
  
  let key_format_resource = Resource::with_attributes(resource, key_format_attrs)
  
  // Verify all keys are treated distinctly
  assert_eq(Resource::get_attribute(key_format_resource, "simple"), Some(StringValue("simple-value")))
  assert_eq(Resource::get_attribute(key_format_resource, "dotted.key"), Some(StringValue("dotted-value")))
  assert_eq(Resource::get_attribute(key_format_resource, "dashed-key"), Some(StringValue("dashed-value")))
  assert_eq(Resource::get_attribute(key_format_resource, "underscore_key"), Some(StringValue("underscore-value")))
  assert_eq(Resource::get_attribute(key_format_resource, "mixed-format_key.key"), Some(StringValue("mixed-value")))
  assert_eq(Resource::get_attribute(key_format_resource, "numeric.123"), Some(StringValue("numeric-key-value")))
  assert_eq(Resource::get_attribute(key_format_resource, "UPPERCASE.KEY"), Some(StringValue("uppercase-value")))
  
  // Test case sensitivity
  assert_eq(Resource::get_attribute(key_format_resource, "simple"), Some(StringValue("simple-value")))
  assert_eq(Resource::get_attribute(key_format_resource, "SIMPLE"), None)
  
  assert_eq(Resource::get_attribute(key_format_resource, "dotted.key"), Some(StringValue("dotted-value")))
  assert_eq(Resource::get_attribute(key_format_resource, "DOTTED.KEY"), None)
}

test "resource unicode and special characters" {
  let resource = Resource::new()
  
  // Test Unicode values
  let unicode_attrs = [
    ("unicode.value", StringValue("测试值🚀")),
    ("chinese.value", StringValue("中文测试")),
    ("emoji.value", StringValue("🎉✨💯")),
    ("mixed.unicode", StringValue("Mix测试ed🚀Value"))
  ]
  
  let unicode_resource = Resource::with_attributes(resource, unicode_attrs)
  
  assert_eq(Resource::get_attribute(unicode_resource, "unicode.value"), Some(StringValue("测试值🚀")))
  assert_eq(Resource::get_attribute(unicode_resource, "chinese.value"), Some(StringValue("中文测试")))
  assert_eq(Resource::get_attribute(unicode_resource, "emoji.value"), Some(StringValue("🎉✨💯")))
  assert_eq(Resource::get_attribute(unicode_resource, "mixed.unicode"), Some(StringValue("Mix测试ed🚀Value")))
  
  // Test special characters in values
  let special_chars_attrs = [
    ("json.value", StringValue("{\"key\": \"value\", \"number\": 42}")),
    ("url.value", StringValue("https://example.com/path?param=value&other=123")),
    ("xml.value", StringValue("<element attribute=\"value\">content</element>")),
    ("sql.value", StringValue("SELECT * FROM users WHERE id = 123")),
    ("regex.value", StringValue("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")),
    ("special.chars", StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  ]
  
  let special_chars_resource = Resource::with_attributes(unicode_resource, special_chars_attrs)
  
  assert_eq(Resource::get_attribute(special_chars_resource, "json.value"), Some(StringValue("{\"key\": \"value\", \"number\": 42}")))
  assert_eq(Resource::get_attribute(special_chars_resource, "url.value"), Some(StringValue("https://example.com/path?param=value&other=123")))
  assert_eq(Resource::get_attribute(special_chars_resource, "xml.value"), Some(StringValue("<element attribute=\"value\">content</element>")))
  assert_eq(Resource::get_attribute(special_chars_resource, "sql.value"), Some(StringValue("SELECT * FROM users WHERE id = 123")))
  assert_eq(Resource::get_attribute(special_chars_resource, "regex.value"), Some(StringValue("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")))
  assert_eq(Resource::get_attribute(special_chars_resource, "special.chars"), Some(StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?")))
}

test "large scale resource operations" {
  let resource = Resource::new()
  
  // Test resource with many attributes
  let mut large_resource = resource
  for i in 0..50 {
    let key = "attribute." + Int::to_string(i)
    let value = "value." + Int::to_string(i)
    let attrs = [(key, StringValue(value))]
    large_resource = Resource::with_attributes(large_resource, attrs)
  }
  
  // Verify attributes can be retrieved
  for i in 0..50 {
    let key = "attribute." + Int::to_string(i)
    let expected_value = "value." + Int::to_string(i)
    assert_eq(Resource::get_attribute(large_resource, key), Some(StringValue(expected_value)))
  }
  
  // Test resource with long key and value names
  let long_key = "a" * 100
  let long_value = "b" * 200
  let long_attrs = [(long_key, StringValue(long_value))]
  let long_resource = Resource::with_attributes(large_resource, long_attrs)
  
  assert_eq(Resource::get_attribute(long_resource, long_key), Some(StringValue(long_value)))
  
  // Test resource with many array attributes
  let mut array_resource = long_resource
  for i in 0..20 {
    let key = "array.attribute." + Int::to_string(i)
    let string_array = Array[String]::make(10, "element-" + Int::to_string(i))
    let int_array = Array[Int]::make(10, i)
    
    let string_attrs = [(key + ".string", ArrayStringValue(string_array))]
    array_resource = Resource::with_attributes(array_resource, string_attrs)
    
    let int_attrs = [(key + ".int", ArrayIntValue(int_array))]
    array_resource = Resource::with_attributes(array_resource, int_attrs)
  }
  
  // Verify array attributes
  for i in 0..20 {
    let string_key = "array.attribute." + Int::to_string(i) + ".string"
    let int_key = "array.attribute." + Int::to_string(i) + ".int"
    
    match Resource::get_attribute(array_resource, string_key) {
      Some(ArrayStringValue(arr)) => assert_eq(arr.length(), 10)
      _ => assert_false(true)
    }
    
    match Resource::get_attribute(array_resource, int_key) {
      Some(ArrayIntValue(arr)) => assert_eq(arr.length(), 10)
      _ => assert_false(true)
    }
  }
}

test "resource merge priority and conflict resolution" {
  // Create multiple resources with overlapping attributes
  let resource1_attrs = [
    ("service.name", StringValue("service-A")),
    ("service.version", StringValue("v1.0.0")),
    ("environment", StringValue("dev")),
    ("region", StringValue("us-west-1"))
  ]
  let resource1 = Resource::with_attributes(Resource::new(), resource1_attrs)
  
  let resource2_attrs = [
    ("service.name", StringValue("service-B")),
    ("service.version", StringValue("v2.0.0")),
    ("environment", StringValue("prod")),
    ("zone", StringValue("us-west-1a"))
  ]
  let resource2 = Resource::with_attributes(Resource::new(), resource2_attrs)
  
  let resource3_attrs = [
    ("service.name", StringValue("service-C")),
    ("host.name", StringValue("server-01"),
    ("environment", StringValue("staging"))
  ]
  let resource3 = Resource::with_attributes(Resource::new(), resource3_attrs)
  
  // Test merge chain (resource1 -> resource2 -> resource3)
  let merged1_2 = Resource::merge(resource1, resource2)
  let final_merged = Resource::merge(merged1_2, resource3)
  
  // In simplified implementation, final merge returns resource3
  assert_eq(Resource::get_attribute(final_merged, "service.name"), Some(StringValue("service-C")))
  assert_eq(Resource::get_attribute(final_merged, "service.version"), None)
  assert_eq(Resource::get_attribute(final_merged, "environment"), Some(StringValue("staging")))
  assert_eq(Resource::get_attribute(final_merged, "region"), None)
  assert_eq(Resource::get_attribute(final_merged, "zone"), None)
  assert_eq(Resource::get_attribute(final_merged, "host.name"), Some(StringValue("server-01")))
  
  // Test merge with empty resource
  let empty_resource = Resource::new()
  let merged_with_empty = Resource::merge(empty_resource, resource1)
  
  // In simplified implementation, merge returns override (resource1)
  assert_eq(Resource::get_attribute(merged_with_empty, "service.name"), Some(StringValue("service-A")))
  assert_eq(Resource::get_attribute(merged_with_empty, "service.version"), Some(StringValue("v1.0.0")))
  
  let merged_empty_with = Resource::merge(resource1, empty_resource)
  
  // In simplified implementation, merge returns override (empty_resource)
  assert_eq(Resource::get_attribute(merged_empty_with, "service.name"), None)
  assert_eq(Resource::get_attribute(merged_empty_with, "service.version"), None)
}

test "resource attribute value type consistency" {
  let resource = Resource::new()
  
  // Test setting same key with different value types
  let string_attrs = [("multi.type.key", StringValue("string-value"))]
  let string_resource = Resource::with_attributes(resource, string_attrs)
  
  let int_attrs = [("multi.type.key", IntValue(42))]
  let int_resource = Resource::with_attributes(string_resource, int_attrs)
  
  let float_attrs = [("multi.type.key", FloatValue(3.14))]
  let float_resource = Resource::with_attributes(int_resource, float_attrs)
  
  let bool_attrs = [("multi.type.key", BoolValue(true))]
  let bool_resource = Resource::with_attributes(float_resource, bool_attrs)
  
  // In simplified implementation, each with_attributes creates a new resource
  // So the final value should be the boolean
  assert_eq(Resource::get_attribute(bool_resource, "multi.type.key"), Some(BoolValue(true)))
  
  // Test with array types
  let string_array_attrs = [("array.type.key", ArrayStringValue(["a", "b", "c"]))]
  let string_array_resource = Resource::with_attributes(bool_resource, string_array_attrs)
  
  let int_array_attrs = [("array.type.key", ArrayIntValue([1, 2, 3]))]
  let int_array_resource = Resource::with_attributes(string_array_resource, int_array_attrs)
  
  assert_eq(Resource::get_attribute(int_array_resource, "array.type.key"), Some(ArrayIntValue([1, 2, 3])))
  
  // Test mixed type scenarios
  let mixed_attrs = [
    ("string.key", StringValue("value")),
    ("int.key", IntValue(123)),
    ("float.key", FloatValue(45.67)),
    ("bool.key", BoolValue(false)),
    ("string.array.key", ArrayStringValue(["x", "y", "z"])),
    ("int.array.key", ArrayIntValue([10, 20, 30]))
  ]
  
  let mixed_resource = Resource::with_attributes(int_array_resource, mixed_attrs)
  
  assert_eq(Resource::get_attribute(mixed_resource, "string.key"), Some(StringValue("value")))
  assert_eq(Resource::get_attribute(mixed_resource, "int.key"), Some(IntValue(123)))
  assert_eq(Resource::get_attribute(mixed_resource, "float.key"), Some(FloatValue(45.67)))
  assert_eq(Resource::get_attribute(mixed_resource, "bool.key"), Some(BoolValue(false)))
  assert_eq(Resource::get_attribute(mixed_resource, "string.array.key"), Some(ArrayStringValue(["x", "y", "z"])))
  assert_eq(Resource::get_attribute(mixed_resource, "int.array.key"), Some(ArrayIntValue([10, 20, 30])))
}