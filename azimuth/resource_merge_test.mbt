// Resource Merge Test Suite for Azimuth Telemetry System
// Tests resource creation, attribute management, and merge operations

test "resource creation and basic operations" {
  let resource = Resource::new()
  
  // Test empty resource
  let missing_attr = Resource::get_attribute(resource, "service.name")
  assert_eq(missing_attr, None)
  
  // Test resource with attributes
  let attributes = [
    ("service.name", StringValue("azimuth")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  assert_eq(service_name, Some(StringValue("azimuth")))
  
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  assert_eq(service_version, Some(StringValue("1.0.0")))
  
  let service_instance = Resource::get_attribute(resource_with_attrs, "service.instance.id")
  assert_eq(service_instance, Some(StringValue("instance-123")))
}

test "resource merge with different attribute types" {
  let base_attrs = [
    ("service.name", StringValue("azimuth")),
    ("service.port", IntValue(8080)),
    ("service.enabled", BoolValue(true))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [
    ("service.name", StringValue("azimuth-updated")),
    ("service.environment", StringValue("production")),
    ("service.retries", IntValue(3))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // Test that override values take precedence
  let merged_name = Resource::get_attribute(merged, "service.name")
  assert_eq(merged_name, Some(StringValue("azimuth-updated")))
  
  // Test that new attributes from override are included
  let merged_env = Resource::get_attribute(merged, "service.environment")
  assert_eq(merged_env, Some(StringValue("production")))
  
  let merged_retries = Resource::get_attribute(merged, "service.retries")
  assert_eq(merged_retries, Some(IntValue(3)))
}

test "resource merge with empty resources" {
  let base_attrs = [
    ("service.name", StringValue("azimuth")),
    ("service.version", StringValue("1.0.0"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let empty_resource = Resource::new()
  
  // Test merging with empty override
  let merged1 = Resource::merge(base_resource, empty_resource)
  let service_name = Resource::get_attribute(merged1, "service.name")
  assert_eq(service_name, Some(StringValue("azimuth")))
  
  // Test merging with empty base
  let merged2 = Resource::merge(empty_resource, base_resource)
  let service_name2 = Resource::get_attribute(merged2, "service.name")
  assert_eq(service_name2, Some(StringValue("azimuth")))
}

test "resource merge with complex attributes" {
  let base_attrs = [
    ("service.tags", ArrayStringValue(["web", "api", "microservice"])),
    ("service.metrics", ArrayIntValue([100, 200, 300])),
    ("service.uptime", FloatValue(99.99))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [
    ("service.tags", ArrayStringValue(["web", "api", "v2"])),
    ("service.response.time", FloatValue(150.5)),
    ("service.status", StringValue("healthy"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // Test that array values are overridden
  let merged_tags = Resource::get_attribute(merged, "service.tags")
  assert_eq(merged_tags, Some(ArrayStringValue(["web", "api", "v2"])))
  
  // Test that new attributes are added
  let merged_response_time = Resource::get_attribute(merged, "service.response.time")
  assert_eq(merged_response_time, Some(FloatValue(150.5)))
  
  let merged_status = Resource::get_attribute(merged, "service.status")
  assert_eq(merged_status, Some(StringValue("healthy")))
}

test "resource merge with special characters and edge cases" {
  let base_attrs = [
    ("service.name-with-dashes", StringValue("test-service")),
    ("service.name_with_underscores", StringValue("test_service")),
    ("service.name.with.dots", StringValue("test.service")),
    ("service.empty.string", StringValue("")),
    ("service.zero.value", IntValue(0))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [
    ("service.name-with-dashes", StringValue("updated-service")),
    ("service.new.attribute", StringValue("new-value")),
    ("service.null.test", StringValue("null"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // Test attributes with special characters
  let merged_dashes = Resource::get_attribute(merged, "service.name-with-dashes")
  assert_eq(merged_dashes, Some(StringValue("updated-service")))
  
  let merged_underscores = Resource::get_attribute(merged, "service.name_with_underscores")
  assert_eq(merged_underscores, Some(StringValue("test_service")))
  
  let merged_dots = Resource::get_attribute(merged, "service.name.with.dots")
  assert_eq(merged_dots, Some(StringValue("test.service")))
  
  // Test empty and zero values are preserved
  let merged_empty = Resource::get_attribute(merged, "service.empty.string")
  assert_eq(merged_empty, Some(StringValue("")))
  
  let merged_zero = Resource::get_attribute(merged, "service.zero.value")
  assert_eq(merged_zero, Some(IntValue(0)))
  
  // Test new attributes are added
  let merged_new = Resource::get_attribute(merged, "service.new.attribute")
  assert_eq(merged_new, Some(StringValue("new-value")))
}