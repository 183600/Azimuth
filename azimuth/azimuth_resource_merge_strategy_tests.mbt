// Resource Attribute Merge Strategy Tests for Azimuth
// This file contains test cases for different resource attribute merge strategies

test "basic resource attribute merge" {
  // Test basic resource attribute merging
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("base-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("deployment.environment", azimuth::StringValue("development"))
  ])
  
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.version", azimuth::StringValue("2.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-123"))
  ])
  
  // Merge resources
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // Verify merge results (override takes precedence)
  let service_name = azimuth::Resource::get_attribute(merged_resource, "service.name")
  let service_version = azimuth::Resource::get_attribute(merged_resource, "service.version")
  let environment = azimuth::Resource::get_attribute(merged_resource, "deployment.environment")
  let instance_id = azimuth::Resource::get_attribute(merged_resource, "service.instance.id")
  
  // Base attributes should be preserved if not overridden
  assert_eq(service_name, Some(azimuth::StringValue("base-service")))
  assert_eq(environment, Some(azimuth::StringValue("development")))
  
  // Override attributes should take precedence
  assert_eq(service_version, Some(azimuth::StringValue("2.0.0")))
  assert_eq(instance_id, Some(azimuth::StringValue("instance-123")))
}

test "resource attribute merge with different types" {
  // Test resource attribute merging with different attribute value types
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("type-test-service")),
    ("numeric.value", azimuth::IntValue(42)),
    ("float.value", azimuth::FloatValue(3.14)),
    ("boolean.value", azimuth::BoolValue(true)),
    ("array.value", azimuth::ArrayStringValue(["item1", "item2"]))
  ])
  
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("numeric.value", azimuth::FloatValue(42.0)),  // Different type
    ("boolean.value", azimuth::BoolValue(false)),  // Same type, different value
    ("array.value", azimuth::ArrayIntValue([1, 2, 3])),  // Different array type
    ("new.string.value", azimuth::StringValue("new value"))
  ])
  
  // Merge resources
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // Verify merge results
  let service_name = azimuth::Resource::get_attribute(merged_resource, "service.name")
  let numeric_value = azimuth::Resource::get_attribute(merged_resource, "numeric.value")
  let float_value = azimuth::Resource::get_attribute(merged_resource, "float.value")
  let boolean_value = azimuth::Resource::get_attribute(merged_resource, "boolean.value")
  let array_value = azimuth::Resource::get_attribute(merged_resource, "array.value")
  let new_string_value = azimuth::Resource::get_attribute(merged_resource, "new.string.value")
  
  // Non-overridden attributes should be preserved
  assert_eq(service_name, Some(azimuth::StringValue("type-test-service")))
  assert_eq(float_value, Some(azimuth::FloatValue(3.14)))
  
  // Overridden attributes should use override values
  assert_eq(numeric_value, Some(azimuth::FloatValue(42.0)))
  assert_eq(boolean_value, Some(azimuth::BoolValue(false)))
  assert_eq(array_value, Some(azimuth::ArrayIntValue([1, 2, 3])))
  assert_eq(new_string_value, Some(azimuth::StringValue("new value")))
}

test "resource merge with null values" {
  // Test resource merging when override resource has null/empty attributes
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("null-test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("deployment.environment", azimuth::StringValue("production"))
  ])
  
  let override_resource = azimuth::Resource::new()  // Empty resource
  
  // Merge resources
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // Verify all base attributes are preserved
  let service_name = azimuth::Resource::get_attribute(merged_resource, "service.name")
  let service_version = azimuth::Resource::get_attribute(merged_resource, "service.version")
  let environment = azimuth::Resource::get_attribute(merged_resource, "deployment.environment")
  
  assert_eq(service_name, Some(azimuth::StringValue("null-test-service")))
  assert_eq(service_version, Some(azimuth::StringValue("1.0.0")))
  assert_eq(environment, Some(azimuth::StringValue("production")))
}

test "resource merge with empty base" {
  // Test resource merging when base resource is empty
  let base_resource = azimuth::Resource::new()  // Empty resource
  
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("empty-base-service")),
    ("service.version", azimuth::StringValue("2.0.0")),
    ("deployment.environment", azimuth::StringValue("staging"))
  ])
  
  // Merge resources
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // Verify all override attributes are present
  let service_name = azimuth::Resource::get_attribute(merged_resource, "service.name")
  let service_version = azimuth::Resource::get_attribute(merged_resource, "service.version")
  let environment = azimuth::Resource::get_attribute(merged_resource, "deployment.environment")
  
  assert_eq(service_name, Some(azimuth::StringValue("empty-base-service")))
  assert_eq(service_version, Some(azimuth::StringValue("2.0.0")))
  assert_eq(environment, Some(azimuth::StringValue("staging")))
}

test "resource merge with identical attributes" {
  // Test resource merging when both resources have identical attributes
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("identical-service")),
    ("service.version", azimuth::StringValue("1.5.0")),
    ("deployment.environment", azimuth::StringValue("production"))
  ])
  
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("identical-service")),  // Same value
    ("service.version", azimuth::StringValue("1.5.0")),  // Same value
    ("deployment.environment", azimuth::StringValue("production"))  // Same value
  ])
  
  // Merge resources
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // Verify attributes are preserved
  let service_name = azimuth::Resource::get_attribute(merged_resource, "service.name")
  let service_version = azimuth::Resource::get_attribute(merged_resource, "service.version")
  let environment = azimuth::Resource::get_attribute(merged_resource, "deployment.environment")
  
  assert_eq(service_name, Some(azimuth::StringValue("identical-service")))
  assert_eq(service_version, Some(azimuth::StringValue("1.5.0")))
  assert_eq(environment, Some(azimuth::StringValue("production")))
}

test "resource merge with large attribute sets" {
  // Test resource merging with large numbers of attributes
  let base_attrs = []
  for i in 0..50 {
    base_attrs.push(("base.attr." + i.to_string(), azimuth::IntValue(i)))
  }
  
  let override_attrs = []
  for i in 0..50 {
    override_attrs.push(("override.attr." + i.to_string(), azimuth::StringValue("value." + i.to_string())))
  }
  
  // Add some overlapping attributes
  base_attrs.push(("shared.attr", azimuth::IntValue(100)))
  override_attrs.push(("shared.attr", azimuth::StringValue("overridden")))
  
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), base_attrs)
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), override_attrs)
  
  // Merge resources
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // Verify base-specific attributes are preserved
  for i in 0..50 {
    let base_attr = azimuth::Resource::get_attribute(merged_resource, "base.attr." + i.to_string())
    assert_eq(base_attr, Some(azimuth::IntValue(i)))
  }
  
  // Verify override-specific attributes are present
  for i in 0..50 {
    let override_attr = azimuth::Resource::get_attribute(merged_resource, "override.attr." + i.to_string())
    assert_eq(override_attr, Some(azimuth::StringValue("value." + i.to_string())))
  }
  
  // Verify shared attribute is overridden
  let shared_attr = azimuth::Resource::get_attribute(merged_resource, "shared.attr")
  assert_eq(shared_attr, Some(azimuth::StringValue("overridden")))
}

test "resource merge with special characters" {
  // Test resource merging with special characters in attribute names and values
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("special-chars-service")),
    ("attr.with.dots", azimuth::StringValue("dots.value")),
    ("attr_with_underscores", azimuth::StringValue("underscores.value")),
    ("attr-with-hyphens", azimuth::StringValue("hyphens.value")),
    ("attr with spaces", azimuth::StringValue("spaces.value")),
    ("attr.with.symbols!@#$%", azimuth::StringValue("symbols.value"))
  ])
  
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("attr.with.dots", azimuth::StringValue("overridden.dots")),
    ("new.attr.with.dots", azimuth::StringValue("new.dots.value")),
    ("attr.with.symbols!@#$%", azimuth::StringValue("overridden.symbols"))
  ])
  
  // Merge resources
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // Verify merge results with special characters
  let service_name = azimuth::Resource::get_attribute(merged_resource, "service.name")
  let dots_attr = azimuth::Resource::get_attribute(merged_resource, "attr.with.dots")
  let underscores_attr = azimuth::Resource::get_attribute(merged_resource, "attr_with_underscores")
  let hyphens_attr = azimuth::Resource::get_attribute(merged_resource, "attr-with-hyphens")
  let spaces_attr = azimuth::Resource::get_attribute(merged_resource, "attr with spaces")
  let symbols_attr = azimuth::Resource::get_attribute(merged_resource, "attr.with.symbols!@#$%")
  let new_dots_attr = azimuth::Resource::get_attribute(merged_resource, "new.attr.with.dots")
  
  // Non-overridden attributes should be preserved
  assert_eq(service_name, Some(azimuth::StringValue("special-chars-service")))
  assert_eq(underscores_attr, Some(azimuth::StringValue("underscores.value")))
  assert_eq(hyphens_attr, Some(azimuth::StringValue("hyphens.value")))
  assert_eq(spaces_attr, Some(azimuth::StringValue("spaces.value")))
  
  // Overridden attributes should use override values
  assert_eq(dots_attr, Some(azimuth::StringValue("overridden.dots")))
  assert_eq(symbols_attr, Some(azimuth::StringValue("overridden.symbols")))
  
  // New attributes should be present
  assert_eq(new_dots_attr, Some(azimuth::StringValue("new.dots.value")))
}

test "resource merge with array attributes" {
  // Test resource merging with array attributes
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("array-service")),
    ("string.array", azimuth::ArrayStringValue(["base1", "base2", "base3"])),
    ("int.array", azimuth::ArrayIntValue([1, 2, 3, 4, 5]))
  ])
  
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("string.array", azimuth::ArrayStringValue(["override1", "override2"])),
    ("new.array", azimuth::ArrayStringValue(["new1", "new2", "new3"]))
  ])
  
  // Merge resources
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // Verify merge results
  let service_name = azimuth::Resource::get_attribute(merged_resource, "service.name")
  let string_array = azimuth::Resource::get_attribute(merged_resource, "string.array")
  let int_array = azimuth::Resource::get_attribute(merged_resource, "int.array")
  let new_array = azimuth::Resource::get_attribute(merged_resource, "new.array")
  
  // Non-overridden attributes should be preserved
  assert_eq(service_name, Some(azimuth::StringValue("array-service")))
  assert_eq(int_array, Some(azimuth::ArrayIntValue([1, 2, 3, 4, 5])))
  
  // Overridden arrays should use override values
  assert_eq(string_array, Some(azimuth::ArrayStringValue(["override1", "override2"])))
  
  // New arrays should be present
  assert_eq(new_array, Some(azimuth::ArrayStringValue(["new1", "new2", "new3"])))
}

test "resource merge priority strategy" {
  // Test different merge priority strategies
  
  // Strategy 1: Override takes precedence (default)
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("priority-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("priority.attr", azimuth::StringValue("base.value"))
  ])
  
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.version", azimuth::StringValue("2.0.0")),
    ("priority.attr", azimuth::StringValue("override.value"))
  ])
  
  // Merge with override priority
  let merged_override_priority = azimuth::Resource::merge(base_resource, override_resource)
  
  // Verify override priority results
  let service_name = azimuth::Resource::get_attribute(merged_override_priority, "service.name")
  let service_version = azimuth::Resource::get_attribute(merged_override_priority, "service.version")
  let priority_attr = azimuth::Resource::get_attribute(merged_override_priority, "priority.attr")
  
  assert_eq(service_name, Some(azimuth::StringValue("priority-service")))
  assert_eq(service_version, Some(azimuth::StringValue("2.0.0")))
  assert_eq(priority_attr, Some(azimuth::StringValue("override.value")))
  
  // Strategy 2: Base takes precedence (simulated by reversing merge order)
  let merged_base_priority = azimuth::Resource::merge(override_resource, base_resource)
  
  // Verify base priority results
  let service_name_bp = azimuth::Resource::get_attribute(merged_base_priority, "service.name")
  let service_version_bp = azimuth::Resource::get_attribute(merged_base_priority, "service.version")
  let priority_attr_bp = azimuth::Resource::get_attribute(merged_base_priority, "priority.attr")
  
  assert_eq(service_name_bp, Some(azimuth::StringValue("priority-service")))
  assert_eq(service_version_bp, Some(azimuth::StringValue("1.0.0")))
  assert_eq(priority_attr_bp, Some(azimuth::StringValue("base.value")))
}

test "resource merge with cascading merges" {
  // Test cascading resource merges (multiple resources merged in sequence)
  let resource1 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("cascade-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("layer1.attr", azimuth::StringValue("layer1.value"))
  ])
  
  let resource2 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.version", azimuth::StringValue("1.5.0")),
    ("layer2.attr", azimuth::StringValue("layer2.value"))
  ])
  
  let resource3 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.version", azimuth::StringValue("2.0.0")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("layer3.attr", azimuth::StringValue("layer3.value"))
  ])
  
  // Cascade merge: resource1 -> resource2 -> resource3
  let merged12 = azimuth::Resource::merge(resource1, resource2)
  let merged123 = azimuth::Resource::merge(merged12, resource3)
  
  // Verify cascade merge results
  let service_name = azimuth::Resource::get_attribute(merged123, "service.name")
  let service_version = azimuth::Resource::get_attribute(merged123, "service.version")
  let environment = azimuth::Resource::get_attribute(merged123, "deployment.environment")
  let layer1_attr = azimuth::Resource::get_attribute(merged123, "layer1.attr")
  let layer2_attr = azimuth::Resource::get_attribute(merged123, "layer2.attr")
  let layer3_attr = azimuth::Resource::get_attribute(merged123, "layer3.attr")
  
  // Verify all attributes are present with correct precedence
  assert_eq(service_name, Some(azimuth::StringValue("cascade-service")))
  assert_eq(service_version, Some(azimuth::StringValue("2.0.0")))  // From resource3
  assert_eq(environment, Some(azimuth::StringValue("production")))  // From resource3
  assert_eq(layer1_attr, Some(azimuth::StringValue("layer1.value")))  // From resource1
  assert_eq(layer2_attr, Some(azimuth::StringValue("layer2.value")))  // From resource2
  assert_eq(layer3_attr, Some(azimuth::StringValue("layer3.value")))  // From resource3
}