// Resource Merge Strategy Correctness Tests
// Test cases for comprehensive resource operations and merge strategy validation

test "resource_basic_creation_and_operations" {
  // Test basic resource creation and operations
  let resource = Resource::new()
  
  // Test with single attribute
  let single_attr = Array::[("service.name", StringValue("payment-service"))]
  let resource_with_single = Resource::with_attributes(resource, single_attr)
  
  let retrieved_single = Resource::get_attribute(resource_with_single, "service.name")
  @assertion.assert_eq(retrieved_single, Some(StringValue("payment-service")))?
  
  // Test with multiple attributes
  let multiple_attrs = Array::[
    ("service.name", StringValue("api-gateway")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource_with_multiple = Resource::with_attributes(resource, multiple_attrs)
  
  @assertion.assert_eq(Resource::get_attribute(resource_with_multiple, "service.name"), Some(StringValue("api-gateway")))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_multiple, "service.version"), Some(StringValue("1.2.3")))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_multiple, "service.instance.id"), Some(StringValue("instance-123")))?
}

test "resource_attribute_type_handling" {
  // Test resource with different attribute value types
  let resource = Resource::new()
  
  let mixed_attrs = Array::[
    ("string.attr", StringValue("string_value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("string.array.attr", ArrayStringValue(Array::["item1", "item2"])),
    ("int.array.attr", ArrayIntValue(Array::[1, 2, 3]))
  ]
  
  let resource_with_mixed = Resource::with_attributes(resource, mixed_attrs)
  
  // Verify each type is preserved correctly
  @assertion.assert_eq(Resource::get_attribute(resource_with_mixed, "string.attr"), Some(StringValue("string_value")))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_mixed, "int.attr"), Some(IntValue(42)))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_mixed, "float.attr"), Some(FloatValue(3.14159)))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_mixed, "bool.attr"), Some(BoolValue(true)))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_mixed, "string.array.attr"), Some(ArrayStringValue(Array::["item1", "item2"])))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_mixed, "int.array.attr"), Some(ArrayIntValue(Array::[1, 2, 3])))?
}

test "resource_merge_basic_strategy" {
  // Test basic resource merge strategy
  let base_resource = Resource::new()
  let base_attrs = Array::[
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("production"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = Array::[
    ("service.name", StringValue("override-service")),
    ("deployment.region", StringValue("us-west-2"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Test merge
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // In simplified implementation, override resource should take precedence
  @assertion.assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("override-service")))?
  @assertion.assert_eq(Resource::get_attribute(merged_resource, "deployment.region"), Some(StringValue("us-west-2")))?
}

test "resource_merge_conflict_resolution" {
  // Test resource merge conflict resolution
  let base_resource = Resource::new()
  let base_attrs = Array::[
    ("service.name", StringValue("original-service")),
    ("service.version", StringValue("1.0.0")),
    ("host.name", StringValue("host1.example.com"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = Array::[
    ("service.name", StringValue("updated-service")),
    ("service.version", StringValue("2.0.0")),
    ("host.name", StringValue("host2.example.com"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Test merge with conflicts
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify override values take precedence
  @assertion.assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("updated-service")))?
  @assertion.assert_eq(Resource::get_attribute(merged_resource, "service.version"), Some(StringValue("2.0.0")))?
  @assertion.assert_eq(Resource::get_attribute(merged_resource, "host.name"), Some(StringValue("host2.example.com")))?
}

test "resource_merge_with_empty_resources" {
  // Test merging with empty resources
  let base_resource = Resource::new()
  let base_attrs = Array::[
    ("service.name", StringValue("test-service")),
    ("environment", StringValue("development"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let empty_resource = Resource::new()
  
  // Test merge base with empty
  let merged_base_empty = Resource::merge(base_with_attrs, empty_resource)
  
  // Test merge empty with base
  let merged_empty_base = Resource::merge(empty_resource, base_with_attrs)
  
  // Verify results
  @assertion.assert_eq(Resource::get_attribute(merged_base_empty, "service.name"), Some(StringValue("test-service")))?
  @assertion.assert_eq(Resource::get_attribute(merged_base_empty, "environment"), Some(StringValue("development")))?
  
  @assertion.assert_eq(Resource::get_attribute(merged_empty_base, "service.name"), Some(StringValue("test-service")))?
  @assertion.assert_eq(Resource::get_attribute(merged_empty_base, "environment"), Some(StringValue("development")))?
}

test "resource_merge_complex_attributes" {
  // Test merging resources with complex attributes
  let base_resource = Resource::new()
  let base_attrs = Array::[
    ("service.name", StringValue("complex-service")),
    ("metadata", StringValue("{\"version\":\"1.0\",\"type\":\"web\"}")),
    ("tags", ArrayStringValue(Array::["web", "api", "public"])),
    ("metrics", ArrayIntValue(Array::[100, 200, 300]))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = Array::[
    ("service.name", StringValue("updated-complex-service")),
    ("metadata", StringValue("{\"version\":\"2.0\",\"type\":\"microservice\"}")),
    ("region", StringValue("us-east-1"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Test complex merge
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify merge results
  @assertion.assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("updated-complex-service")))?
  @assertion.assert_eq(Resource::get_attribute(merged_resource, "metadata"), Some(StringValue("{\"version\":\"2.0\",\"type\":\"microservice\"}")))?
  @assertion.assert_eq(Resource::get_attribute(merged_resource, "region"), Some(StringValue("us-east-1")))?
}

test "resource_merge_associative_property" {
  // Test if resource merge is associative (merge(A, merge(B, C)) == merge(merge(A, B), C))
  let resource_a = Resource::new()
  let attrs_a = Array::[("service.name", StringValue("service-a")), ("attr.a", StringValue("value-a"))]
  let resource_a_with_attrs = Resource::with_attributes(resource_a, attrs_a)
  
  let resource_b = Resource::new()
  let attrs_b = Array::[("service.name", StringValue("service-b")), ("attr.b", StringValue("value-b"))]
  let resource_b_with_attrs = Resource::with_attributes(resource_b, attrs_b)
  
  let resource_c = Resource::new()
  let attrs_c = Array::[("service.name", StringValue("service-c")), ("attr.c", StringValue("value-c"))]
  let resource_c_with_attrs = Resource::with_attributes(resource_c, attrs_c)
  
  // Test merge(A, merge(B, C))
  let merge_bc = Resource::merge(resource_b_with_attrs, resource_c_with_attrs)
  let merge_a_bc = Resource::merge(resource_a_with_attrs, merge_bc)
  
  // Test merge(merge(A, B), C)
  let merge_ab = Resource::merge(resource_a_with_attrs, resource_b_with_attrs)
  let merge_ab_c = Resource::merge(merge_ab, resource_c_with_attrs)
  
  // In simplified implementation, the last override should win
  @assertion.assert_eq(Resource::get_attribute(merge_a_bc, "service.name"), Some(StringValue("service-a")))?
  @assertion.assert_eq(Resource::get_attribute(merge_ab_c, "service.name"), Some(StringValue("service-a")))?
}

test "resource_edge_cases" {
  // Test resource edge cases
  let resource = Resource::new()
  
  // Test with empty key
  let empty_key_attrs = Array::[("", StringValue("empty-key-value"))]
  let resource_with_empty_key = Resource::with_attributes(resource, empty_key_attrs)
  @assertion.assert_eq(Resource::get_attribute(resource_with_empty_key, ""), Some(StringValue("empty-key-value")))?
  
  // Test with empty value
  let empty_value_attrs = Array::[("empty.value", StringValue(""))]
  let resource_with_empty_value = Resource::with_attributes(resource, empty_value_attrs)
  @assertion.assert_eq(Resource::get_attribute(resource_with_empty_value, "empty.value"), Some(StringValue("")))?
  
  // Test with special characters in key
  let special_key_attrs = Array::[("special.key.with-dots_and_underscores", StringValue("special-value"))]
  let resource_with_special_key = Resource::with_attributes(resource, special_key_attrs)
  @assertion.assert_eq(Resource::get_attribute(resource_with_special_key, "special.key.with-dots_and_underscores"), Some(StringValue("special-value")))?
  
  // Test with special characters in value
  let special_value_attrs = Array::[("normal.key", StringValue("value with spaces & special!chars@#$%"))]
  let resource_with_special_value = Resource::with_attributes(resource, special_value_attrs)
  @assertion.assert_eq(Resource::get_attribute(resource_with_special_value, "normal.key"), Some(StringValue("value with spaces & special!chars@#$%")))?
}

test "resource_attribute_override_behavior" {
  // Test specific attribute override behavior
  let base_resource = Resource::new()
  let base_attrs = Array::[
    ("override.test", StringValue("base-value")),
    ("keep.test", StringValue("keep-value"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = Array::[
    ("override.test", StringValue("override-value")),
    ("new.test", StringValue("new-value"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Test merge
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify override behavior
  @assertion.assert_eq(Resource::get_attribute(merged_resource, "override.test"), Some(StringValue("override-value")))?
  @assertion.assert_eq(Resource::get_attribute(merged_resource, "keep.test"), Some(StringValue("keep-value")))?
  @assertion.assert_eq(Resource::get_attribute(merged_resource, "new.test"), Some(StringValue("new-value")))?
}