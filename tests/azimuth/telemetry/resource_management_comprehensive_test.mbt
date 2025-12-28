test "resource creation and basic operations" {
  // Test resource creation
  let resource = Resource::new()
  
  // Test getting non-existent attribute
  let non_existent = Resource::get_attribute(resource, "non.existent")
  assert_eq(non_existent, None)
}

test "resource with attributes" {
  // Test resource creation with attributes
  let resource = Resource::new()
  
  // Create attributes array
  let attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("localhost")),
    ("process.pid", IntValue(1234)),
    ("process.memory.usage", FloatValue(256.5))
  ]
  
  // Create resource with attributes
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  // Test getting specific attributes
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let process_pid = Resource::get_attribute(resource_with_attrs, "process.pid")
  
  assert_eq(service_name, Some(StringValue("azimuth-service")))
  assert_eq(process_pid, Some(IntValue(1234)))
}

test "resource merge operations" {
  // Test resource merging
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("environment", StringValue("production"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Test merged resource attributes
  let service_name = Resource::get_attribute(merged, "service.name")
  let environment = Resource::get_attribute(merged, "environment")
  
  assert_eq(service_name, Some(StringValue("override-service")))
  assert_eq(environment, Some(StringValue("production")))
}

test "resource with array attributes" {
  // Test resource with array attributes
  let resource = Resource::new()
  
  let array_attrs = [
    ("service.tags", ArrayStringValue(["web", "api", "microservice"])),
    ("deployment.regions", ArrayStringValue(["us-east-1", "us-west-2"])),
    ("port.numbers", ArrayIntValue([8080, 8443, 9090]))
  ]
  
  let resource_with_arrays = Resource::with_attributes(resource, array_attrs)
  
  // Test getting array attributes
  let service_tags = Resource::get_attribute(resource_with_arrays, "service.tags")
  let port_numbers = Resource::get_attribute(resource_with_arrays, "port.numbers")
  
  assert_eq(service_tags, Some(ArrayStringValue(["web", "api", "microservice"])))
  assert_eq(port_numbers, Some(ArrayIntValue([8080, 8443, 9090])))
}

test "resource with special characters" {
  // Test resource with special characters in attribute keys and values
  let resource = Resource::new()
  
  let special_attrs = [
    ("service.name-with-dots", StringValue("service.value")),
    ("service.name_with_underscores", StringValue("underscore_value")),
    ("service.name-with-hyphens", StringValue("hyphen_value")),
    ("service.unicode.测试", StringValue("unicode.值")),
    ("service.emoji", StringValue("🚀 service"))
  ]
  
  let resource_with_special = Resource::with_attributes(resource, special_attrs)
  
  // Test getting special attributes
  let unicode_attr = Resource::get_attribute(resource_with_special, "service.unicode.测试")
  let emoji_attr = Resource::get_attribute(resource_with_special, "service.emoji")
  
  assert_eq(unicode_attr, Some(StringValue("unicode.值")))
  assert_eq(emoji_attr, Some(StringValue("🚀 service")))
}

test "resource with empty and null values" {
  // Test resource with empty and null-like values
  let resource = Resource::new()
  
  let empty_attrs = [
    ("empty.string", StringValue("")),
    ("empty.array", ArrayStringValue([])),
    ("zero.int", IntValue(0)),
    ("zero.float", FloatValue(0.0)),
    ("false.bool", BoolValue(false))
  ]
  
  let resource_with_empty = Resource::with_attributes(resource, empty_attrs)
  
  // Test getting empty values
  let empty_string = Resource::get_attribute(resource_with_empty, "empty.string")
  let empty_array = Resource::get_attribute(resource_with_empty, "empty.array")
  let zero_int = Resource::get_attribute(resource_with_empty, "zero.int")
  
  assert_eq(empty_string, Some(StringValue("")))
  assert_eq(empty_array, Some(ArrayStringValue([])))
  assert_eq(zero_int, Some(IntValue(0)))
}