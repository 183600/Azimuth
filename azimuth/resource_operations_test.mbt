test "Resource operations comprehensive test" {
  // Test resource creation and attribute operations
  let resource = Resource::new()
  @assertion.assert_eq(resource.attributes.length, 0)?
  
  // Test resource with attributes
  let attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("test-host")),
    ("process.pid", IntValue(12345)),
    ("process.cpu.percent", FloatValue(75.5)),
    ("process.debug.enabled", BoolValue(true))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  @assertion.assert_eq(resource_with_attrs.attributes.length, 7)?
  
  // Test attribute retrieval
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  @assertion.assert_eq(service_name?, StringValue("azimuth-service"))?
  
  let process_pid = Resource::get_attribute(resource_with_attrs, "process.pid")
  @assertion.assert_eq(process_pid?, IntValue(12345))?
  
  let cpu_percent = Resource::get_attribute(resource_with_attrs, "process.cpu.percent")
  @assertion.assert_eq(cpu_percent?, FloatValue(75.5))?
  
  let debug_enabled = Resource::get_attribute(resource_with_attrs, "process.debug.enabled")
  @assertion.assert_eq(debug_enabled?, BoolValue(true))?
  
  // Test non-existent attribute
  let non_existent = Resource::get_attribute(resource_with_attrs, "non.existent.key")
  @assertion.assert_eq(non_existent, None)?
  
  // Test resource merging
  let override_attrs = [
    ("service.name", StringValue("updated-service")),
    ("new.attribute", StringValue("new-value"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  
  // Verify merge results (simplified implementation returns override)
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  @assertion.assert_eq(merged_service_name?, StringValue("updated-service"))?
  
  let new_attr = Resource::get_attribute(merged_resource, "new.attribute")
  @assertion.assert_eq(new_attr?, StringValue("new-value"))?
}

test "Resource attribute type validation test" {
  // Test different attribute value types
  let resource = Resource::new()
  
  // String array attributes
  let string_array_attrs = [
    ("service.tags", ArrayStringValue(["tag1", "tag2", "tag3"]))
  ]
  let resource_with_strings = Resource::with_attributes(resource, string_array_attrs)
  let tags = Resource::get_attribute(resource_with_strings, "service.tags")
  @assertion.assert_eq(tags?, ArrayStringValue(["tag1", "tag2", "tag3"]))?
  
  // Int array attributes
  let int_array_attrs = [
    ("service.ports", ArrayIntValue([8080, 8081, 8082]))
  ]
  let resource_with_ints = Resource::with_attributes(resource_with_strings, int_array_attrs)
  let ports = Resource::get_attribute(resource_with_ints, "service.ports")
  @assertion.assert_eq(ports?, ArrayIntValue([8080, 8081, 8082]))?
  
  // Mixed attribute types in single resource
  let mixed_attrs = [
    ("string.attr", StringValue("test")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14)),
    ("bool.attr", BoolValue(true)),
    ("string.array", ArrayStringValue(["a", "b"])),
    ("int.array", ArrayIntValue([1, 2, 3]))
  ]
  let mixed_resource = Resource::with_attributes(Resource::new(), mixed_attrs)
  @assertion.assert_eq(mixed_resource.attributes.length, 6)?
  
  // Verify each attribute type
  @assertion.assert_eq(Resource::get_attribute(mixed_resource, "string.attr")?, StringValue("test"))?
  @assertion.assert_eq(Resource::get_attribute(mixed_resource, "int.attr")?, IntValue(42))?
  @assertion.assert_eq(Resource::get_attribute(mixed_resource, "float.attr")?, FloatValue(3.14))?
  @assertion.assert_eq(Resource::get_attribute(mixed_resource, "bool.attr")?, BoolValue(true))?
  @assertion.assert_eq(Resource::get_attribute(mixed_resource, "string.array")?, ArrayStringValue(["a", "b"]))?
  @assertion.assert_eq(Resource::get_attribute(mixed_resource, "int.array")?, ArrayIntValue([1, 2, 3]))?
}