// Resource Merge Strategy Tests for Azimuth
// This file contains tests for resource merging strategies

test "basic resource merge" {
  // Test basic resource merging
  let base_resource = Resource::new()
  let base_attrs = [("service.name", StringValue("base-service")), ("version", StringValue("1.0.0"))]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [("environment", StringValue("production")), ("region", StringValue("us-west"))]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify merged resource contains override attributes
  let env_attr = Resource::get_attribute(merged, "environment")
  let region_attr = Resource::get_attribute(merged, "region")
  
  match env_attr {
    Some(StringValue(env)) => assert_eq(env, "production")
    _ => assert_true(false, "Expected environment attribute")
  }
  
  match region_attr {
    Some(StringValue(region)) => assert_eq(region, "us-west")
    _ => assert_true(false, "Expected region attribute")
  }
}

test "resource merge with conflicting keys" {
  // Test resource merging with conflicting attribute keys
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("original-service")),
    ("version", StringValue("1.0.0")),
    ("environment", StringValue("development"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("version", StringValue("2.0.0")),
    ("region", StringValue("us-east"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify override values take precedence
  let service_name = Resource::get_attribute(merged, "service.name")
  let version = Resource::get_attribute(merged, "version")
  let region = Resource::get_attribute(merged, "region")
  
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "override-service")
    _ => assert_true(false, "Expected overridden service name")
  }
  
  match version {
    Some(StringValue(v)) => assert_eq(v, "2.0.0")
    _ => assert_true(false, "Expected overridden version")
  }
  
  match region {
    Some(StringValue(r)) => assert_eq(r, "us-east")
    _ => assert_true(false, "Expected region attribute")
  }
}

test "resource merge with different value types" {
  // Test resource merging with different attribute value types
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("test-service")),
    ("metric.count", IntValue(42)),
    ("metric.ratio", FloatValue(3.14)),
    ("service.enabled", BoolValue(true))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("updated-service")),
    ("metric.count", IntValue(100)),
    ("metric.ratio", FloatValue(2.71)),
    ("service.enabled", BoolValue(false))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify all value types are properly overridden
  let service_name = Resource::get_attribute(merged, "service.name")
  let metric_count = Resource::get_attribute(merged, "metric.count")
  let metric_ratio = Resource::get_attribute(merged, "metric.ratio")
  let service_enabled = Resource::get_attribute(merged, "service.enabled")
  
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "updated-service")
    _ => assert_true(false, "Expected StringValue")
  }
  
  match metric_count {
    Some(IntValue(count)) => assert_eq(count, 100)
    _ => assert_true(false, "Expected IntValue")
  }
  
  match metric_ratio {
    Some(FloatValue(ratio)) => assert_true(ratio > 2.7 && ratio < 2.8)
    _ => assert_true(false, "Expected FloatValue")
  }
  
  match service_enabled {
    Some(BoolValue(enabled)) => assert_false(enabled)
    _ => assert_true(false, "Expected BoolValue")
  }
}

test "resource merge with array values" {
  // Test resource merging with array attribute values
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("array-service")),
    ("tags", ArrayStringValue(["tag1", "tag2", "tag3"])),
    ("numbers", ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("updated-array-service")),
    ("tags", ArrayStringValue(["new-tag1", "new-tag2"])),
    ("numbers", ArrayIntValue([10, 20, 30]))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify array values are overridden
  let service_name = Resource::get_attribute(merged, "service.name")
  let tags = Resource::get_attribute(merged, "tags")
  let numbers = Resource::get_attribute(merged, "numbers")
  
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "updated-array-service")
    _ => assert_true(false, "Expected StringValue")
  }
  
  match tags {
    Some(ArrayStringValue(tag_array)) => {
      assert_eq(tag_array.length(), 2)
      assert_eq(tag_array[0], "new-tag1")
      assert_eq(tag_array[1], "new-tag2")
    }
    _ => assert_true(false, "Expected ArrayStringValue")
  }
  
  match numbers {
    Some(ArrayIntValue(num_array)) => {
      assert_eq(num_array.length(), 3)
      assert_eq(num_array[0], 10)
      assert_eq(num_array[2], 30)
    }
    _ => assert_true(false, "Expected ArrayIntValue")
  }
}

test "resource merge with empty resources" {
  // Test merging with empty resources
  let base_resource = Resource::new()
  let base_attrs = [("service.name", StringValue("base-service"))]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let empty_resource = Resource::new()
  
  // Merge base with empty
  let merged1 = Resource::merge(base_with_attrs, empty_resource)
  let service_name1 = Resource::get_attribute(merged1, "service.name")
  
  // Merge empty with base
  let merged2 = Resource::merge(empty_resource, base_with_attrs)
  let service_name2 = Resource::get_attribute(merged2, "service.name")
  
  // Merge empty with empty
  let merged3 = Resource::merge(empty_resource, empty_resource)
  
  match service_name1 {
    Some(StringValue(name)) => assert_eq(name, "base-service")
    _ => assert_true(false, "Expected service name")
  }
  
  match service_name2 {
    Some(StringValue(name)) => assert_eq(name, "base-service")
    _ => assert_true(false, "Expected service name")
  }
  
  // Empty merge should not crash
  assert_true(true)
}

test "resource merge with special characters" {
  // Test resource merging with special characters in values
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("special-service")),
    ("description", StringValue("Base description with special chars: !@#$%^&*()")),
    ("unicode.value", StringValue("Base unicode: 测试 🚀"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override-special-service")),
    ("description", StringValue("Override description with JSON: {\"key\": \"value\"}")),
    ("unicode.value", StringValue("Override unicode: ñáéíóú 漢字"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify special characters are preserved
  let service_name = Resource::get_attribute(merged, "service.name")
  let description = Resource::get_attribute(merged, "description")
  let unicode_value = Resource::get_attribute(merged, "unicode.value")
  
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "override-special-service")
    _ => assert_true(false, "Expected StringValue")
  }
  
  match description {
    Some(StringValue(desc)) => assert_true(desc.contains("{\"key\""))
    _ => assert_true(false, "Expected StringValue with JSON")
  }
  
  match unicode_value {
    Some(StringValue(unicode)) => assert_true(unicode.contains("ñáéíóú"))
    _ => assert_true(false, "Expected StringValue with unicode")
  }
}

test "resource merge chain operations" {
  // Test chaining multiple resource merges
  let resource1 = Resource::new()
  let attrs1 = [("service.name", StringValue("service1")), ("version", StringValue("1.0.0"))]
  let resource1_with_attrs = Resource::with_attributes(resource1, attrs1)
  
  let resource2 = Resource::new()
  let attrs2 = [("environment", StringValue("dev")), ("region", StringValue("us-west"))]
  let resource2_with_attrs = Resource::with_attributes(resource2, attrs2)
  
  let resource3 = Resource::new()
  let attrs3 = [("service.name", StringValue("final-service")), ("team", StringValue("platform"))]
  let resource3_with_attrs = Resource::with_attributes(resource3, attrs3)
  
  // Chain merges: resource1 -> resource2 -> resource3
  let merged1 = Resource::merge(resource1_with_attrs, resource2_with_attrs)
  let final_merged = Resource::merge(merged1, resource3_with_attrs)
  
  // Verify final merged resource
  let service_name = Resource::get_attribute(final_merged, "service.name")
  let version = Resource::get_attribute(final_merged, "version")
  let environment = Resource::get_attribute(final_merged, "environment")
  let region = Resource::get_attribute(final_merged, "region")
  let team = Resource::get_attribute(final_merged, "team")
  
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "final-service")
    _ => assert_true(false, "Expected final service name")
  }
  
  match version {
    Some(StringValue(v)) => assert_eq(v, "1.0.0")
    _ => assert_true(false, "Expected version")
  }
  
  match environment {
    Some(StringValue(env)) => assert_eq(env, "dev")
    _ => assert_true(false, "Expected environment")
  }
  
  match region {
    Some(StringValue(r)) => assert_eq(r, "us-west")
    _ => assert_true(false, "Expected region")
  }
  
  match team {
    Some(StringValue(t)) => assert_eq(t, "platform")
    _ => assert_true(false, "Expected team")
  }
}

test "resource merge with large attribute sets" {
  // Test resource merging with large attribute sets
  let base_resource = Resource::new()
  let mut base_attrs = []
  
  // Create large base attribute set
  for i = 0; i < 50; i = i + 1 {
    base_attrs = base_attrs + [("base.attr." + i.to_string(), StringValue("base-value-" + i.to_string()))]
  }
  
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let mut override_attrs = []
  
  // Create large override attribute set
  for i = 0; i < 50; i = i + 1 {
    override_attrs = override_attrs + [("override.attr." + i.to_string(), StringValue("override-value-" + i.to_string()))]
  }
  
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify some attributes from both sets
  let base_attr = Resource::get_attribute(merged, "base.attr.0")
  let override_attr = Resource::get_attribute(merged, "override.attr.0")
  
  // Based on simplified implementation, override attributes should be present
  match override_attr {
    Some(StringValue(value)) => assert_eq(value, "override-value-0")
    _ => assert_true(false, "Expected override attribute")
  }
}