test "resource creation and basic operations" {
  // Test resource creation
  let resource = @azimuth.Resource::new()
  
  // Test getting non-existent attribute
  let non_existent = @azimuth.Resource::get_attribute(resource, "non.existent")
  assert_eq(non_existent, None)
}

test "resource with attributes" {
  // Test resource creation with attributes
  let resource = @azimuth.Resource::new()
  
  // Create attributes array
  let attributes = [
    ("service.name", @azimuth.StringValue("azimuth-service")),
    ("service.version", @azimuth.StringValue("1.0.0")),
    ("service.instance.id", @azimuth.StringValue("instance-123")),
    ("host.name", @azimuth.StringValue("localhost")),
    ("process.pid", @azimuth.IntValue(1234)),
    ("process.memory.usage", @azimuth.FloatValue(256.5))
  ]
  
  // Create resource with attributes
  let resource_with_attrs = @azimuth.Resource::with_attributes(resource, attributes)
  
  // Test getting specific attributes
  let service_name = @azimuth.Resource::get_attribute(resource_with_attrs, "service.name")
  let process_pid = @azimuth.Resource::get_attribute(resource_with_attrs, "process.pid")
  
  assert_eq(service_name, Some(@azimuth.StringValue("azimuth-service")))
  assert_eq(process_pid, Some(@azimuth.IntValue(1234)))
}

test "resource merge operations" {
  // Test resource merging
  let base_resource = @azimuth.Resource::new()
  let base_attrs = [
    ("service.name", @azimuth.StringValue("base-service")),
    ("service.version", @azimuth.StringValue("1.0.0"))
  ]
  let base_with_attrs = @azimuth.Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = @azimuth.Resource::new()
  let override_attrs = [
    ("service.name", @azimuth.StringValue("override-service")),
    ("environment", @azimuth.StringValue("production"))
  ]
  let override_with_attrs = @azimuth.Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources
  let merged = @azimuth.Resource::merge(base_with_attrs, override_with_attrs)
  
  // Test merged resource attributes
  let service_name = @azimuth.Resource::get_attribute(merged, "service.name")
  let environment = @azimuth.Resource::get_attribute(merged, "environment")
  
  assert_eq(service_name, Some(@azimuth.StringValue("override-service")))
  assert_eq(environment, Some(@azimuth.StringValue("production")))
}

test "resource with array attributes" {
  // Test resource with array attributes
  let resource = @azimuth.Resource::new()
  
  let array_attrs = [
    ("service.tags", @azimuth.ArrayStringValue(["web", "api", "microservice"])),
    ("deployment.regions", @azimuth.ArrayStringValue(["us-east-1", "us-west-2"])),
    ("port.numbers", @azimuth.ArrayIntValue([8080, 8443, 9090]))
  ]
  
  let resource_with_arrays = @azimuth.Resource::with_attributes(resource, array_attrs)
  
  // Test getting array attributes
  let service_tags = @azimuth.Resource::get_attribute(resource_with_arrays, "service.tags")
  let port_numbers = @azimuth.Resource::get_attribute(resource_with_arrays, "port.numbers")
  
  assert_eq(service_tags, Some(@azimuth.ArrayStringValue(["web", "api", "microservice"])))
  assert_eq(port_numbers, Some(@azimuth.ArrayIntValue([8080, 8443, 9090])))
}

test "resource with special characters" {
  // Test resource with special characters in attribute keys and values
  let resource = @azimuth.Resource::new()
  
  let special_attrs = [
    ("service.name-with-dots", @azimuth.StringValue("service.value")),
    ("service.name_with_underscores", @azimuth.StringValue("underscore_value")),
    ("service.name-with-hyphens", @azimuth.StringValue("hyphen_value")),
    ("service.unicode.测试", @azimuth.StringValue("unicode.值")),
    ("service.emoji", @azimuth.StringValue("🚀 service"))
  ]
  
  let resource_with_special = @azimuth.Resource::with_attributes(resource, special_attrs)
  
  // Test getting special attributes
  let unicode_attr = @azimuth.Resource::get_attribute(resource_with_special, "service.unicode.测试")
  let emoji_attr = @azimuth.Resource::get_attribute(resource_with_special, "service.emoji")
  
  assert_eq(unicode_attr, Some(@azimuth.StringValue("unicode.值")))
  assert_eq(emoji_attr, Some(@azimuth.StringValue("🚀 service")))
}

test "resource with empty and null values" {
  // Test resource with empty and null-like values
  let resource = @azimuth.Resource::new()
  
  let empty_attrs = [
    ("empty.string", @azimuth.StringValue("")),
    ("empty.array", @azimuth.ArrayStringValue([])),
    ("zero.int", @azimuth.IntValue(0)),
    ("zero.float", @azimuth.FloatValue(0.0)),
    ("false.bool", @azimuth.BoolValue(false))
  ]
  
  let resource_with_empty = @azimuth.Resource::with_attributes(resource, empty_attrs)
  
  // Test getting empty values
  let empty_string = @azimuth.Resource::get_attribute(resource_with_empty, "empty.string")
  let empty_array = @azimuth.Resource::get_attribute(resource_with_empty, "empty.array")
  let zero_int = @azimuth.Resource::get_attribute(resource_with_empty, "zero.int")
  
  assert_eq(empty_string, Some(@azimuth.StringValue("")))
  assert_eq(empty_array, Some(@azimuth.ArrayStringValue([])))
  assert_eq(zero_int, Some(@azimuth.IntValue(0)))
}