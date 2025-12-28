// Azimuth Enhanced Resource Operations Test Suite

test "resource_creation_and_attribute_management" {
  // Test empty resource creation
  let empty_resource = Resource::new()
  assert_eq(empty_resource.attributes.length, 0)
  
  // Test resource with attributes
  let attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("production-server")),
    ("process.pid", IntValue(1234))
  ]
  
  let resource_with_attrs = Resource::with_attributes(empty_resource, attrs)
  
  // Test attribute retrieval
  match Resource::get_attribute(resource_with_attrs, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "azimuth-service")
    _ => @expect(false)
  }
  
  match Resource::get_attribute(resource_with_attrs, "process.pid") {
    Some(IntValue(pid)) => assert_eq(pid, 1234)
    _ => @expect(false)
  }
  
  // Test non-existent attribute
  assert_eq(Resource::get_attribute(resource_with_attrs, "non.existent.key"), None)
}

test "resource_merge_operations" {
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("development"))
  ]
  
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("host.name", StringValue("new-host")),
    ("feature.flag", BoolValue(true))
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // Verify override takes precedence
  match Resource::get_attribute(merged_resource, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "override-service")
    _ => @expect(false)
  }
  
  // Verify new attributes from override are included
  match Resource::get_attribute(merged_resource, "host.name") {
    Some(StringValue(host)) => assert_eq(host, "new-host")
    _ => @expect(false)
  }
  
  match Resource::get_attribute(merged_resource, "feature.flag") {
    Some(BoolValue(flag)) => assert_eq(flag, true)
    _ => @expect(false)
  }
}

test "resource_attribute_type_variations" {
  let resource = Resource::with_attributes(Resource::new(), [
    ("string.attr", StringValue("test-string")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("array.string.attr", ArrayStringValue(["item1", "item2", "item3"])),
    ("array.int.attr", ArrayIntValue([1, 2, 3, 4, 5]))
  ])
  
  // Test string attribute
  match Resource::get_attribute(resource, "string.attr") {
    Some(StringValue(value)) => assert_eq(value, "test-string")
    _ => @expect(false)
  }
  
  // Test integer attribute
  match Resource::get_attribute(resource, "int.attr") {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => @expect(false)
  }
  
  // Test float attribute
  match Resource::get_attribute(resource, "float.attr") {
    Some(FloatValue(value)) => assert_eq(value, 3.14159)
    _ => @expect(false)
  }
  
  // Test boolean attribute
  match Resource::get_attribute(resource, "bool.attr") {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => @expect(false)
  }
  
  // Test string array attribute
  match Resource::get_attribute(resource, "array.string.attr") {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length, 3)
      assert_eq(values[0], "item1")
      assert_eq(values[1], "item2")
      assert_eq(values[2], "item3")
    }
    _ => @expect(false)
  }
  
  // Test integer array attribute
  match Resource::get_attribute(resource, "array.int.attr") {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length, 5)
      assert_eq(values[0], 1)
      assert_eq(values[4], 5)
    }
    _ => @expect(false)
  }
}

test "resource_edge_cases" {
  // Test resource with empty string attributes
  let empty_string_resource = Resource::with_attributes(Resource::new(), [
    ("empty.string", StringValue("")),
    ("normal.string", StringValue("normal"))
  ])
  
  match Resource::get_attribute(empty_string_resource, "empty.string") {
    Some(StringValue(value)) => assert_eq(value, "")
    _ => @expect(false)
  }
  
  // Test resource with zero and negative values
  let numeric_edge_resource = Resource::with_attributes(Resource::new(), [
    ("zero.int", IntValue(0)),
    ("negative.int", IntValue(-42)),
    ("zero.float", FloatValue(0.0)),
    ("negative.float", FloatValue(-3.14))
  ])
  
  match Resource::get_attribute(numeric_edge_resource, "zero.int") {
    Some(IntValue(value)) => assert_eq(value, 0)
    _ => @expect(false)
  }
  
  match Resource::get_attribute(numeric_edge_resource, "negative.int") {
    Some(IntValue(value)) => assert_eq(value, -42)
    _ => @expect(false)
  }
  
  match Resource::get_attribute(numeric_edge_resource, "zero.float") {
    Some(FloatValue(value)) => assert_eq(value, 0.0)
    _ => @expect(false)
  }
  
  match Resource::get_attribute(numeric_edge_resource, "negative.float") {
    Some(FloatValue(value)) => assert_eq(value, -3.14)
    _ => @expect(false)
  }
}