// Enhanced Resource Operations Test Suite for Azimuth Telemetry System
// This file contains comprehensive test cases for resource management and operations

test "resource creation and basic operations" {
  let resource = Resource::new()
  assert_true(resource.attributes.length == 0)
  
  let attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  assert_eq(service_name, Some(StringValue("azimuth-service")))
  
  let missing_attr = Resource::get_attribute(resource_with_attrs, "missing.attribute")
  assert_eq(missing_attr, None)
}

test "resource merge operations" {
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("environment", StringValue("development"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("region", StringValue("us-west-2"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // In simplified implementation, override resource is returned
  let service_name = Resource::get_attribute(merged, "service.name")
  assert_eq(service_name, Some(StringValue("override-service")))
  
  let region = Resource::get_attribute(merged, "region")
  assert_eq(region, Some(StringValue("us-west-2")))
}

test "resource with complex attribute types" {
  let complex_attrs = [
    ("service.startup.time", IntValue(1234567890)),
    ("service.memory.usage", FloatValue(512.5)),
    ("service.production", BoolValue(true)),
    ("service.tags", ArrayStringValue(["web", "api", "microservice"])),
    ("service.ports", ArrayIntValue([8080, 8443, 9090]))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), complex_attrs)
  
  let startup_time = Resource::get_attribute(resource, "service.startup.time")
  assert_eq(startup_time, Some(IntValue(1234567890)))
  
  let memory_usage = Resource::get_attribute(resource, "service.memory.usage")
  assert_eq(memory_usage, Some(FloatValue(512.5)))
  
  let is_production = Resource::get_attribute(resource, "service.production")
  assert_eq(is_production, Some(BoolValue(true)))
  
  let tags = Resource::get_attribute(resource, "service.tags")
  assert_eq(tags, Some(ArrayStringValue(["web", "api", "microservice"])))
  
  let ports = Resource::get_attribute(resource, "service.ports")
  assert_eq(ports, Some(ArrayIntValue([8080, 8443, 9090])))
}

test "resource attribute validation" {
  let resource = Resource::new()
  
  // Test empty resource
  assert_true(resource.attributes.length == 0)
  
  // Test resource with empty attributes array
  let empty_attrs = [] : Array[(String, AttributeValue)]
  let empty_resource = Resource::with_attributes(resource, empty_attrs)
  assert_true(empty_resource.attributes.length == 0)
  
  // Test resource with null-like values
  let null_like_attrs = [
    ("empty.string", StringValue("")),
    ("zero.int", IntValue(0)),
    ("zero.float", FloatValue(0.0)),
    ("false.bool", BoolValue(false))
  ]
  let null_like_resource = Resource::with_attributes(resource, null_like_attrs)
  
  let empty_string = Resource::get_attribute(null_like_resource, "empty.string")
  assert_eq(empty_string, Some(StringValue("")))
  
  let zero_int = Resource::get_attribute(null_like_resource, "zero.int")
  assert_eq(zero_int, Some(IntValue(0)))
}

test "resource with special characters in keys" {
  let special_attrs = [
    ("service.name.with.dots", StringValue("service-1")),
    ("service-name-with-dashes", StringValue("service-2")),
    ("service_name_with_underscores", StringValue("service-3")),
    ("service.name.with-numbers123", StringValue("service-4")),
    ("service.name.with-special.chars!@#", StringValue("service-5"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), special_attrs)
  
  let dot_key = Resource::get_attribute(resource, "service.name.with.dots")
  assert_eq(dot_key, Some(StringValue("service-1")))
  
  let dash_key = Resource::get_attribute(resource, "service-name-with-dashes")
  assert_eq(dash_key, Some(StringValue("service-2")))
  
  let underscore_key = Resource::get_attribute(resource, "service_name_with_underscores")
  assert_eq(underscore_key, Some(StringValue("service-3")))
  
  let number_key = Resource::get_attribute(resource, "service.name.with-numbers123")
  assert_eq(number_key, Some(StringValue("service-4")))
  
  let special_key = Resource::get_attribute(resource, "service.name.with-special.chars!@#")
  assert_eq(special_key, Some(StringValue("service-5")))
}

test "resource large attribute handling" {
  // Test with a large number of attributes
  let large_attrs = Array.range(0, 100).map(i => ("attr." + i.to_string(), StringValue("value-" + i.to_string())))
  let large_resource = Resource::with_attributes(Resource::new(), large_attrs)
  
  // Test retrieving first and last attributes
  let first_attr = Resource::get_attribute(large_resource, "attr.0")
  assert_eq(first_attr, Some(StringValue("value-0")))
  
  let last_attr = Resource::get_attribute(large_resource, "attr.99")
  assert_eq(last_attr, Some(StringValue("value-99")))
  
  // Test retrieving a middle attribute
  let middle_attr = Resource::get_attribute(large_resource, "attr.50")
  assert_eq(middle_attr, Some(StringValue("value-50")))
}

test "resource attribute case sensitivity" {
  let case_attrs = [
    ("service.name", StringValue("lowercase")),
    ("Service.Name", StringValue("mixedcase")),
    ("SERVICE.NAME", StringValue("uppercase"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), case_attrs)
  
  let lowercase = Resource::get_attribute(resource, "service.name")
  assert_eq(lowercase, Some(StringValue("lowercase")))
  
  let mixedcase = Resource::get_attribute(resource, "Service.Name")
  assert_eq(mixedcase, Some(StringValue("mixedcase")))
  
  let uppercase = Resource::get_attribute(resource, "SERVICE.NAME")
  assert_eq(uppercase, Some(StringValue("uppercase")))
}

test "resource attribute overwriting" {
  let resource = Resource::new()
  
  // Add initial attributes
  let initial_attrs = [
    ("service.name", StringValue("initial-service")),
    ("service.version", StringValue("1.0.0"))
  ]
  let resource_v1 = Resource::with_attributes(resource, initial_attrs)
  
  // Overwrite with new attributes
  let updated_attrs = [
    ("service.name", StringValue("updated-service")),
    ("service.environment", StringValue("production"))
  ]
  let resource_v2 = Resource::with_attributes(resource_v1, updated_attrs)
  
  // In simplified implementation, this would create a new resource with updated attributes
  let service_name = Resource::get_attribute(resource_v2, "service.name")
  assert_eq(service_name, Some(StringValue("updated-service")))
}

test "resource empty and null attribute handling" {
  let resource = Resource::new()
  
  // Test with various empty/null-like values
  let empty_attrs = [
    ("empty.string", StringValue("")),
    ("zero.int", IntValue(0)),
    ("zero.float", FloatValue(0.0)),
    ("false.bool", BoolValue(false)),
    ("empty.array.string", ArrayStringValue([])),
    ("empty.array.int", ArrayIntValue([]))
  ]
  
  let resource_with_empty = Resource::with_attributes(resource, empty_attrs)
  
  // Verify all empty attributes can be retrieved
  assert_eq(Resource::get_attribute(resource_with_empty, "empty.string"), Some(StringValue("")))
  assert_eq(Resource::get_attribute(resource_with_empty, "zero.int"), Some(IntValue(0)))
  assert_eq(Resource::get_attribute(resource_with_empty, "zero.float"), Some(FloatValue(0.0)))
  assert_eq(Resource::get_attribute(resource_with_empty, "false.bool"), Some(BoolValue(false)))
  assert_eq(Resource::get_attribute(resource_with_empty, "empty.array.string"), Some(ArrayStringValue([])))
  assert_eq(Resource::get_attribute(resource_with_empty, "empty.array.int"), Some(ArrayIntValue([])))
}

test "resource attribute value type conversion" {
  let resource = Resource::new()
  
  // Test with different numeric types
  let numeric_attrs = [
    ("count.as.int", IntValue(42)),
    ("count.as.float", FloatValue(42.0)),
    ("flag.as.bool", BoolValue(true)),
    ("name.as.string", StringValue("42"))
  ]
  
  let resource_with_numeric = Resource::with_attributes(resource, numeric_attrs)
  
  let int_value = Resource::get_attribute(resource_with_numeric, "count.as.int")
  assert_eq(int_value, Some(IntValue(42)))
  
  let float_value = Resource::get_attribute(resource_with_numeric, "count.as.float")
  assert_eq(float_value, Some(FloatValue(42.0)))
  
  let bool_value = Resource::get_attribute(resource_with_numeric, "flag.as.bool")
  assert_eq(bool_value, Some(BoolValue(true)))
  
  let string_value = Resource::get_attribute(resource_with_numeric, "name.as.string")
  assert_eq(string_value, Some(StringValue("42")))
}