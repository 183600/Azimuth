// Resource Merge Strategy Tests
// This file contains comprehensive tests for resource merge strategies and behaviors

test "basic resource attribute merging" {
  // Test basic resource attribute merging behavior
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override-service")), // Should override
    ("service.instance.id", StringValue("instance-123")), // New attribute
    ("host.name", StringValue("host-001"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify merge results
  let service_name = Resource::get_attribute(merged, "service.name")
  let service_version = Resource::get_attribute(merged, "service.version")
  let environment = Resource::get_attribute(merged, "deployment.environment")
  let instance_id = Resource::get_attribute(merged, "service.instance.id")
  let host_name = Resource::get_attribute(merged, "host.name")
  
  assert_true(service_name != None) // Should have overridden value
  assert_eq(service_version, Some(StringValue("1.0.0"))) // Should preserve base value
  assert_eq(environment, Some(StringValue("production"))) // Should preserve base value
  assert_eq(instance_id, Some(StringValue("instance-123"))) // Should have new value
  assert_eq(host_name, Some(StringValue("host-001"))) // Should have new value
}

test "resource merge with different attribute types" {
  // Test resource merging with different attribute types
  let base_resource = Resource::new()
  let base_attrs = [
    ("string.attr", StringValue("base-string")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14)),
    ("bool.attr", BoolValue(true))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("string.attr", StringValue("override-string")), // Override string
    ("int.attr", IntValue(100)), // Override int
    ("float.attr", FloatValue(2.71)), // Override float
    ("bool.attr", BoolValue(false)), // Override bool
    ("array.string", ArrayStringValue(["a", "b", "c"])) // New array attribute
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify type-specific merging
  let string_attr = Resource::get_attribute(merged, "string.attr")
  let int_attr = Resource::get_attribute(merged, "int.attr")
  let float_attr = Resource::get_attribute(merged, "float.attr")
  let bool_attr = Resource::get_attribute(merged, "bool.attr")
  let array_attr = Resource::get_attribute(merged, "array.string")
  
  assert_eq(string_attr, Some(StringValue("override-string")))
  assert_eq(int_attr, Some(IntValue(100)))
  assert_eq(float_attr, Some(FloatValue(2.71)))
  assert_eq(bool_attr, Some(BoolValue(false)))
  assert_eq(array_attr, Some(ArrayStringValue(["a", "b", "c"])))
}

test "resource merge with empty resources" {
  // Test resource merging with empty resources
  let non_empty_resource = Resource::new()
  let attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0"))
  ]
  let non_empty_with_attrs = Resource::with_attributes(non_empty_resource, attrs)
  
  let empty_resource = Resource::new()
  
  // Merge non-empty with empty
  let merged1 = Resource::merge(non_empty_with_attrs, empty_resource)
  
  // Merge empty with non-empty
  let merged2 = Resource::merge(empty_resource, non_empty_with_attrs)
  
  // Merge empty with empty
  let merged3 = Resource::merge(empty_resource, empty_resource)
  
  // Verify results
  let service_name1 = Resource::get_attribute(merged1, "service.name")
  let service_name2 = Resource::get_attribute(merged2, "service.name")
  let service_name3 = Resource::get_attribute(merged3, "service.name")
  
  assert_eq(service_name1, Some(StringValue("test-service")))
  assert_eq(service_name2, Some(StringValue("test-service")))
  assert_eq(service_name3, None)
}

test "resource merge with large attribute sets" {
  // Test resource merging with large attribute sets
  let base_resource = Resource::new()
  let mut base_attrs = []
  for i = 0; i < 100; i = i + 1 {
    base_attrs = base_attrs.push(("base.attr." + i.to_string(), StringValue("base.value." + i.to_string())))
  }
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let mut override_attrs = []
  for i = 0; i < 50; i = i + 1 {
    override_attrs = override_attrs.push(("base.attr." + i.to_string(), StringValue("override.value." + i.to_string())))
  }
  for i = 0; i < 50; i = i + 1 {
    override_attrs = override_attrs.push(("new.attr." + i.to_string(), StringValue("new.value." + i.to_string())))
  }
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge large attribute sets
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify merge results
  let overridden_attr = Resource::get_attribute(merged, "base.attr.0")
  let preserved_attr = Resource::get_attribute(merged, "base.attr.99")
  let new_attr = Resource::get_attribute(merged, "new.attr.0")
  
  assert_eq(overridden_attr, Some(StringValue("override.value.0")))
  assert_eq(preserved_attr, Some(StringValue("base.value.99")))
  assert_eq(new_attr, Some(StringValue("new.value.0")))
}

test "resource merge with special characters in keys" {
  // Test resource merging with special characters in attribute keys
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("special.key-with.dots_and-underscores", StringValue("base-special")),
    ("unicode.key.测试", StringValue("base-unicode")),
    ("key with spaces", StringValue("base-spaces"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("special.key-with.dots_and-underscores", StringValue("override-special")),
    ("unicode.key.测试", StringValue("override-unicode")),
    ("key with spaces", StringValue("override-spaces")),
    ("emoji.key.🚀", StringValue("emoji-value"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources with special characters
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify special character handling
  let service_name = Resource::get_attribute(merged, "service.name")
  let special_key = Resource::get_attribute(merged, "special.key-with.dots_and-underscores")
  let unicode_key = Resource::get_attribute(merged, "unicode.key.测试")
  let spaces_key = Resource::get_attribute(merged, "key with spaces")
  let emoji_key = Resource::get_attribute(merged, "emoji.key.🚀")
  
  assert_eq(service_name, Some(StringValue("override-service")))
  assert_eq(special_key, Some(StringValue("override-special")))
  assert_eq(unicode_key, Some(StringValue("override-unicode")))
  assert_eq(spaces_key, Some(StringValue("override-spaces")))
  assert_eq(emoji_key, Some(StringValue("emoji-value")))
}

test "resource merge chain behavior" {
  // Test resource merge chain behavior (multiple merges in sequence)
  let initial_resource = Resource::new()
  let initial_attrs = [
    ("service.name", StringValue("initial-service")),
    ("initial.attr", StringValue("initial-value"))
  ]
  let initial_with_attrs = Resource::with_attributes(initial_resource, initial_attrs)
  
  let stage1_resource = Resource::new()
  let stage1_attrs = [
    ("service.name", StringValue("stage1-service")),
    ("stage1.attr", StringValue("stage1-value"))
  ]
  let stage1_with_attrs = Resource::with_attributes(stage1_resource, stage1_attrs)
  
  let stage2_resource = Resource::new()
  let stage2_attrs = [
    ("service.version", StringValue("2.0.0")),
    ("stage2.attr", StringValue("stage2-value"))
  ]
  let stage2_with_attrs = Resource::with_attributes(stage2_resource, stage2_attrs)
  
  let final_resource = Resource::new()
  let final_attrs = [
    ("deployment.environment", StringValue("production")),
    ("final.attr", StringValue("final-value"))
  ]
  let final_with_attrs = Resource::with_attributes(final_resource, final_attrs)
  
  // Chain merges
  let merged1 = Resource::merge(initial_with_attrs, stage1_with_attrs)
  let merged2 = Resource::merge(merged1, stage2_with_attrs)
  let final_merged = Resource::merge(merged2, final_with_attrs)
  
  // Verify chain merge results
  let service_name = Resource::get_attribute(final_merged, "service.name")
  let initial_attr = Resource::get_attribute(final_merged, "initial.attr")
  let stage1_attr = Resource::get_attribute(final_merged, "stage1.attr")
  let service_version = Resource::get_attribute(final_merged, "service.version")
  let stage2_attr = Resource::get_attribute(final_merged, "stage2.attr")
  let environment = Resource::get_attribute(final_merged, "deployment.environment")
  let final_attr = Resource::get_attribute(final_merged, "final.attr")
  
  assert_eq(service_name, Some(StringValue("stage1-service"))) // Overridden by stage1
  assert_eq(initial_attr, Some(StringValue("initial-value"))) // Preserved from initial
  assert_eq(stage1_attr, Some(StringValue("stage1-value"))) // From stage1
  assert_eq(service_version, Some(StringValue("2.0.0"))) // From stage2
  assert_eq(stage2_attr, Some(StringValue("stage2-value"))) // From stage2
  assert_eq(environment, Some(StringValue("production"))) // From final
  assert_eq(final_attr, Some(StringValue("final-value"))) // From final
}

test "resource merge with null and empty values" {
  // Test resource merging with null and empty values
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("empty.string", StringValue("")),
    ("normal.attr", StringValue("normal-value"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("empty.string", StringValue("")), // Override with empty string
    ("null.attr", StringValue("null-value")) // New attribute
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge with null and empty values
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify null and empty value handling
  let service_name = Resource::get_attribute(merged, "service.name")
  let empty_string = Resource::get_attribute(merged, "empty.string")
  let normal_attr = Resource::get_attribute(merged, "normal.attr")
  let null_attr = Resource::get_attribute(merged, "null.attr")
  
  assert_eq(service_name, Some(StringValue("override-service")))
  assert_eq(empty_string, Some(StringValue(""))) // Empty string should be preserved
  assert_eq(normal_attr, Some(StringValue("normal-value"))) // Should be preserved
  assert_eq(null_attr, Some(StringValue("null-value"))) // Should be added
}

test "resource merge with array attributes" {
  // Test resource merging with array attributes
  let base_resource = Resource::new()
  let base_attrs = [
    ("string.array", ArrayStringValue(["a", "b", "c"])),
    ("int.array", ArrayIntValue([1, 2, 3])),
    ("normal.attr", StringValue("normal-value"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("string.array", ArrayStringValue(["x", "y", "z"])), // Override array
    ("int.array", ArrayIntValue([10, 20, 30])), // Override array
    ("new.array", ArrayStringValue(["new", "array"])) // New array
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge with array attributes
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify array attribute merging
  let string_array = Resource::get_attribute(merged, "string.array")
  let int_array = Resource::get_attribute(merged, "int.array")
  let normal_attr = Resource::get_attribute(merged, "normal.attr")
  let new_array = Resource::get_attribute(merged, "new.array")
  
  assert_eq(string_array, Some(ArrayStringValue(["x", "y", "z"]))) // Overridden
  assert_eq(int_array, Some(ArrayIntValue([10, 20, 30]))) // Overridden
  assert_eq(normal_attr, Some(StringValue("normal-value"))) // Preserved
  assert_eq(new_array, Some(ArrayStringValue(["new", "array"]))) // Added
}