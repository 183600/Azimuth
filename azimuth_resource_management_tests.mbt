// Resource Management Tests for Azimuth
// This file contains test cases for resource management and lifecycle operations

test "resource creation and attributes" {
  let resource = Resource::new()
  
  // Test resource with single attribute
  let single_attr = [("service.name", StringValue("test-service"))]
  let resource_with_single = Resource::with_attributes(resource, single_attr)
  
  match Resource::get_attribute(resource_with_single, "service.name") {
    Some(StringValue(v)) => assert_eq(v, "test-service")
    _ => assert_true(false)
  }
  
  // Test resource with multiple attributes
  let multiple_attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("test-host"))
  ]
  let resource_with_multiple = Resource::with_attributes(resource, multiple_attrs)
  
  match Resource::get_attribute(resource_with_multiple, "service.name") {
    Some(StringValue(v)) => assert_eq(v, "test-service")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_multiple, "service.version") {
    Some(StringValue(v)) => assert_eq(v, "1.0.0")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_multiple, "service.instance.id") {
    Some(StringValue(v)) => assert_eq(v, "instance-123")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_multiple, "host.name") {
    Some(StringValue(v)) => assert_eq(v, "test-host")
    _ => assert_true(false)
  }
}

test "resource attribute types" {
  let resource = Resource::new()
  
  // Test different attribute types
  let mixed_attrs = [
    ("string.attr", StringValue("string-value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14)),
    ("bool.attr", BoolValue(true)),
    ("array.string.attr", ArrayStringValue(["value1", "value2"])),
    ("array.int.attr", ArrayIntValue([1, 2, 3]))
  ]
  let resource_with_mixed = Resource::with_attributes(resource, mixed_attrs)
  
  // Test string attribute
  match Resource::get_attribute(resource_with_mixed, "string.attr") {
    Some(StringValue(v)) => assert_eq(v, "string-value")
    _ => assert_true(false)
  }
  
  // Test integer attribute
  match Resource::get_attribute(resource_with_mixed, "int.attr") {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // Test float attribute
  match Resource::get_attribute(resource_with_mixed, "float.attr") {
    Some(FloatValue(v)) => assert_eq(v, 3.14)
    _ => assert_true(false)
  }
  
  // Test boolean attribute
  match Resource::get_attribute(resource_with_mixed, "bool.attr") {
    Some(BoolValue(v)) => assert_true(v)
    _ => assert_true(false)
  }
  
  // Test array string attribute
  match Resource::get_attribute(resource_with_mixed, "array.string.attr") {
    Some(ArrayStringValue(v)) => {
      assert_eq(v.length(), 2)
      assert_eq(v[0], "value1")
      assert_eq(v[1], "value2")
    }
    _ => assert_true(false)
  }
  
  // Test array int attribute
  match Resource::get_attribute(resource_with_mixed, "array.int.attr") {
    Some(ArrayIntValue(v)) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], 1)
      assert_eq(v[1], 2)
      assert_eq(v[2], 3)
    }
    _ => assert_true(false)
  }
}

test "resource merge strategy" {
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("base.only.attr", StringValue("base-value"))
  ]
  let base_resource_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("override.only.attr", StringValue("override-value"))
  ]
  let override_resource_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Test resource merge
  let merged_resource = Resource::merge(base_resource_with_attrs, override_resource_with_attrs)
  
  // Test that override attributes take precedence
  match Resource::get_attribute(merged_resource, "service.name") {
    Some(StringValue(v)) => assert_eq(v, "override-service")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(merged_resource, "override.only.attr") {
    Some(StringValue(v)) => assert_eq(v, "override-value")
    _ => assert_true(false)
  }
}

test "resource lifecycle management" {
  // Test resource creation lifecycle
  let initial_resource = Resource::new()
  
  // Add attributes incrementally
  let step1 = Resource::with_attributes(initial_resource, [("step1", StringValue("value1"))])
  let step2 = Resource::with_attributes(step1, [("step2", StringValue("value2"))])
  let step3 = Resource::with_attributes(step2, [("step3", StringValue("value3"))])
  
  // Verify all attributes are present
  match Resource::get_attribute(step3, "step1") {
    Some(StringValue(v)) => assert_eq(v, "value1")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(step3, "step2") {
    Some(StringValue(v)) => assert_eq(v, "value2")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(step3, "step3") {
    Some(StringValue(v)) => assert_eq(v, "value3")
    _ => assert_true(false)
  }
}

test "resource attribute validation" {
  let resource = Resource::new()
  
  // Test empty key validation (simplified - in real implementation would validate)
  let empty_key_attrs = [("", StringValue("value"))]
  let resource_with_empty_key = Resource::with_attributes(resource, empty_key_attrs)
  
  // Test null value validation (simplified)
  let null_value_attrs = [("key", StringValue(""))]
  let resource_with_null_value = Resource::with_attributes(resource, null_value_attrs)
  
  // Test special characters in keys
  let special_chars_attrs = [("special.key.with.dots", StringValue("special-value"))]
  let resource_with_special_chars = Resource::with_attributes(resource, special_chars_attrs)
  
  match Resource::get_attribute(resource_with_special_chars, "special.key.with.dots") {
    Some(StringValue(v)) => assert_eq(v, "special-value")
    _ => assert_true(false)
  }
}

test "resource memory management" {
  // Test resource creation and cleanup
  let mut resources = []
  
  // Create multiple resources
  for i in 0..=10 {
    let resource = Resource::new()
    let attrs = [("resource.id", StringValue("resource-" + i.to_string()))]
    let resource_with_attrs = Resource::with_attributes(resource, attrs)
    resources.push(resource_with_attrs)
  }
  
  // Verify all resources were created
  assert_eq(resources.length(), 11)
  
  // Verify attributes in all resources
  for i in 0..=10 {
    let resource_id = "resource-" + i.to_string()
    match Resource::get_attribute(resources[i], "resource.id") {
      Some(StringValue(v)) => assert_eq(v, resource_id)
      _ => assert_true(false)
    }
  }
}

test "resource hierarchical attributes" {
  let resource = Resource::new()
  
  // Test hierarchical attribute structure
  let hierarchical_attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.namespace", StringValue("default")),
    ("service.environment", StringValue("production")),
    ("host.name", StringValue("test-host")),
    ("host.ip", StringValue("192.168.1.100")),
    ("host.os.name", StringValue("linux")),
    ("host.os.version", StringValue("5.4.0"))
  ]
  let resource_with_hierarchical = Resource::with_attributes(resource, hierarchical_attrs)
  
  // Test service attributes
  match Resource::get_attribute(resource_with_hierarchical, "service.name") {
    Some(StringValue(v)) => assert_eq(v, "test-service")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_hierarchical, "service.version") {
    Some(StringValue(v)) => assert_eq(v, "1.0.0")
    _ => assert_true(false)
  }
  
  // Test host attributes
  match Resource::get_attribute(resource_with_hierarchical, "host.name") {
    Some(StringValue(v)) => assert_eq(v, "test-host")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_hierarchical, "host.os.name") {
    Some(StringValue(v)) => assert_eq(v, "linux")
    _ => assert_true(false)
  }
}

test "resource attribute limits" {
  let resource = Resource::new()
  
  // Test attribute key length limits
  let long_key = "a".repeat(100)
  let long_key_attrs = [(long_key, StringValue("value"))]
  let resource_with_long_key = Resource::with_attributes(resource, long_key_attrs)
  
  match Resource::get_attribute(resource_with_long_key, long_key) {
    Some(StringValue(v)) => assert_eq(v, "value")
    _ => assert_true(false)
  }
  
  // Test attribute value length limits
  let long_value = "v".repeat(1000)
  let long_value_attrs = [("long.value.key", StringValue(long_value))]
  let resource_with_long_value = Resource::with_attributes(resource, long_value_attrs)
  
  match Resource::get_attribute(resource_with_long_value, "long.value.key") {
    Some(StringValue(v)) => assert_eq(v, long_value)
    _ => assert_true(false)
  }
}

test "resource attribute encoding" {
  let resource = Resource::new()
  
  // Test special characters in values
  let special_chars_attrs = [
    ("url.value", StringValue("https://example.com/path?param=value&other=test")),
    ("json.value", StringValue("{\"key\":\"value\",\"number\":42}")),
    ("unicode.value", StringValue("测试中文 🌟")),
    ("escape.chars", StringValue("line1\nline2\ttabbed\r\ncarriage"))
  ]
  let resource_with_special_chars = Resource::with_attributes(resource, special_chars_attrs)
  
  match Resource::get_attribute(resource_with_special_chars, "url.value") {
    Some(StringValue(v)) => assert_eq(v, "https://example.com/path?param=value&other=test")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_special_chars, "json.value") {
    Some(StringValue(v)) => assert_eq(v, "{\"key\":\"value\",\"number\":42}")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_special_chars, "unicode.value") {
    Some(StringValue(v)) => assert_eq(v, "测试中文 🌟")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_special_chars, "escape.chars") {
    Some(StringValue(v)) => assert_eq(v, "line1\nline2\ttabbed\r\ncarriage")
    _ => assert_true(false)
  }
}