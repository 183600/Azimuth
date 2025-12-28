// Enhanced Resource Management Tests for Azimuth Telemetry System
// Focus on complex resource attribute operations and edge cases

test "resource_attribute_complex_merge_scenarios" {
  // Test merging resources with overlapping and conflicting attributes
  let base_attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("server-01"))
  ]
  
  let override_attrs = [
    ("service.version", StringValue("2.0.0")),  // Override
    ("deployment.environment", StringValue("staging")),  // Override
    ("service.instance.id", StringValue("instance-123")),  // New
    ("host.name", StringValue("server-02"))  // Override
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged = Resource::merge(base_resource, override_resource)
  
  // Verify overrides took precedence
  let version = Resource::get_attribute(merged, "service.version")
  match version {
    Some(StringValue(v)) => assert_eq(v, "2.0.0")
    _ => assert_true(false)
  }
  
  let environment = Resource::get_attribute(merged, "deployment.environment")
  match environment {
    Some(StringValue(env)) => assert_eq(env, "staging")
    _ => assert_true(false)
  }
  
  // Verify new attribute was added
  let instance_id = Resource::get_attribute(merged, "service.instance.id")
  match instance_id {
    Some(StringValue(id)) => assert_eq(id, "instance-123")
    _ => assert_true(false)
  }
  
  // Verify host.name was overridden
  let host_name = Resource::get_attribute(merged, "host.name")
  match host_name {
    Some(StringValue(host)) => assert_eq(host, "server-02")
    _ => assert_true(false)
  }
}

test "resource_attribute_type_handling" {
  // Test resource with different attribute value types
  let mixed_attrs = [
    ("string.attr", StringValue("test_string")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("array.string", ArrayStringValue(["item1", "item2", "item3"])),
    ("array.int", ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), mixed_attrs)
  
  // Test string attribute
  let string_result = Resource::get_attribute(resource, "string.attr")
  match string_result {
    Some(StringValue(s)) => assert_eq(s, "test_string")
    _ => assert_true(false)
  }
  
  // Test integer attribute
  let int_result = Resource::get_attribute(resource, "int.attr")
  match int_result {
    Some(IntValue(i)) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  // Test float attribute
  let float_result = Resource::get_attribute(resource, "float.attr")
  match float_result {
    Some(FloatValue(f)) => assert_eq(f, 3.14159)
    _ => assert_true(false)
  }
  
  // Test boolean attribute
  let bool_result = Resource::get_attribute(resource, "bool.attr")
  match bool_result {
    Some(BoolValue(b)) => assert_eq(b, true)
    _ => assert_true(false)
  }
  
  // Test array string attribute
  let array_string_result = Resource::get_attribute(resource, "array.string")
  match array_string_result {
    Some(ArrayStringValue(arr)) => assert_eq(arr.length, 3)
    _ => assert_true(false)
  }
  
  // Test array int attribute
  let array_int_result = Resource::get_attribute(resource, "array.int")
  match array_int_result {
    Some(ArrayIntValue(arr)) => assert_eq(arr.length, 5)
    _ => assert_true(false)
  }
}

test "resource_empty_and_null_handling" {
  // Test empty resource operations
  let empty_resource = Resource::new()
  
  // Test getting from empty resource
  let result = Resource::get_attribute(empty_resource, "nonexistent.key")
  assert_true(result is None)
  
  // Test merging with empty resource
  let attrs = [("service.name", StringValue("test-service"))]
  let populated_resource = Resource::with_attributes(Resource::new(), attrs)
  
  // Empty + populated
  let merged1 = Resource::merge(empty_resource, populated_resource)
  let service_name = Resource::get_attribute(merged1, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_true(false)
  }
  
  // Populated + empty
  let merged2 = Resource::merge(populated_resource, empty_resource)
  let service_name2 = Resource::get_attribute(merged2, "service.name")
  match service_name2 {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_true(false)
  }
  
  // Empty + empty
  let merged3 = Resource::merge(empty_resource, empty_resource)
  let result3 = Resource::get_attribute(merged3, "any.key")
  assert_true(result3 is None)
}

test "resource_large_attribute_sets" {
  // Test resource with many attributes
  let mut large_attrs = []
  
  // Create 50 attributes
  for i = 0; i < 50; i = i + 1 {
    large_attrs = Array::push(large_attrs, ("attr." @ i.to_string(), StringValue("value." @ i.to_string())))
  }
  
  let large_resource = Resource::with_attributes(Resource::new(), large_attrs)
  
  // Test specific attributes
  let first_attr = Resource::get_attribute(large_resource, "attr.0")
  match first_attr {
    Some(StringValue(value)) => assert_eq(value, "value.0")
    _ => assert_true(false)
  }
  
  let last_attr = Resource::get_attribute(large_resource, "attr.49")
  match last_attr {
    Some(StringValue(value)) => assert_eq(value, "value.49")
    _ => assert_true(false)
  }
  
  let middle_attr = Resource::get_attribute(large_resource, "attr.25")
  match middle_attr {
    Some(StringValue(value)) => assert_eq(value, "value.25")
    _ => assert_true(false)
  }
  
  // Test non-existent attribute
  let non_existent = Resource::get_attribute(large_resource, "attr.100")
  assert_true(non_existent is None)
}

test "resource_special_characters_in_keys" {
  // Test resource attributes with special characters in keys
  let special_attrs = [
    ("service.name", StringValue("normal-service")),
    ("service.name-with-dash", StringValue("dash-service")),
    ("service.name_with_underscore", StringValue("underscore-service")),
    ("service.name.with.dots", StringValue("dots-service")),
    ("service.name/with/slashes", StringValue("slash-service")),
    ("service.name\\with\\backslashes", StringValue("backslash-service")),
    ("service.name@with@symbols", StringValue("symbol-service")),
    ("service.name with spaces", StringValue("space-service")),
    ("service.name:with:colons", StringValue("colon-service"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), special_attrs)
  
  // Test each special character key
  let dash_result = Resource::get_attribute(resource, "service.name-with-dash")
  match dash_result {
    Some(StringValue(name)) => assert_eq(name, "dash-service")
    _ => assert_true(false)
  }
  
  let underscore_result = Resource::get_attribute(resource, "service.name_with_underscore")
  match underscore_result {
    Some(StringValue(name)) => assert_eq(name, "underscore-service")
    _ => assert_true(false)
  }
  
  let dots_result = Resource::get_attribute(resource, "service.name.with.dots")
  match dots_result {
    Some(StringValue(name)) => assert_eq(name, "dots-service")
    _ => assert_true(false)
  }
}