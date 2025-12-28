// Resource Merge Strategy Test Suite for Azimuth Telemetry System
// Tests resource attribute merging, override behavior, and conflict resolution

test "resource basic attribute merging" {
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  
  // Add attributes to first resource
  let attrs1 = [("service.name", StringValue("payment-service")), 
                ("service.version", StringValue("1.0.0"))]
  let with_attrs1 = Resource::with_attributes(resource1, attrs1)
  
  // Add attributes to second resource  
  let attrs2 = [("environment", StringValue("production")), 
                ("deployment.region", StringValue("us-west-2"))]
  let with_attrs2 = Resource::with_attributes(resource2, attrs2)
  
  // Merge resources
  let merged = Resource::merge(with_attrs1, with_attrs2)
  
  // Verify all attributes are present
  assert_eq(Resource::get_attribute(merged, "service.name"), Some(StringValue("payment-service")))
  assert_eq(Resource::get_attribute(merged, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(merged, "environment"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(merged, "deployment.region"), Some(StringValue("us-west-2")))
}

test "resource attribute override behavior" {
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  
  // Add attributes to first resource
  let attrs1 = [("service.name", StringValue("original-service")), 
                ("environment", StringValue("development"))]
  let with_attrs1 = Resource::with_attributes(resource1, attrs1)
  
  // Add conflicting attributes to second resource
  let attrs2 = [("service.name", StringValue("override-service")), 
                ("service.version", StringValue("2.0.0"))]
  let with_attrs2 = Resource::with_attributes(resource2, attrs2)
  
  // Merge resources - second resource should override
  let merged = Resource::merge(with_attrs1, with_attrs2)
  
  // Verify overrides
  assert_eq(Resource::get_attribute(merged, "service.name"), Some(StringValue("override-service")))
  assert_eq(Resource::get_attribute(merged, "environment"), Some(StringValue("development")))
  assert_eq(Resource::get_attribute(merged, "service.version"), Some(StringValue("2.0.0")))
}

test "resource multiple merge operations" {
  let base_resource = Resource::new()
  
  // First merge
  let attrs1 = [("service.name", StringValue("base-service")), 
                ("version", StringValue("1.0.0"))]
  let resource1 = Resource::with_attributes(base_resource, attrs1)
  
  // Second merge
  let attrs2 = [("environment", StringValue("staging")), 
                ("region", StringValue("us-east-1"))]
  let resource2 = Resource::with_attributes(resource1, attrs2)
  
  // Third merge
  let attrs3 = [("service.name", StringValue("final-service")), 
                ("feature.flags", ArrayStringValue(["feature1", "feature2"]))]
  let resource3 = Resource::with_attributes(resource2, attrs3)
  
  // Verify final state
  assert_eq(Resource::get_attribute(resource3, "service.name"), Some(StringValue("final-service")))
  assert_eq(Resource::get_attribute(resource3, "version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(resource3, "environment"), Some(StringValue("staging")))
  assert_eq(Resource::get_attribute(resource3, "region"), Some(StringValue("us-east-1")))
  assert_eq(Resource::get_attribute(resource3, "feature.flags"), Some(ArrayStringValue(["feature1", "feature2"])))
}

test "resource with different attribute types" {
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  
  // Mix of different attribute types in first resource
  let attrs1 = [
    ("string.attr", StringValue("string value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("array.string", ArrayStringValue(["item1", "item2"])),
    ("array.int", ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  let with_attrs1 = Resource::with_attributes(resource1, attrs1)
  
  // Mix of different attribute types in second resource
  let attrs2 = [
    ("string.attr", StringValue("overridden string")),
    ("another.int", IntValue(999)),
    ("another.float", FloatValue(2.71828)),
    ("bool.attr", BoolValue(false))
  ]
  let with_attrs2 = Resource::with_attributes(resource2, attrs2)
  
  // Merge and verify
  let merged = Resource::merge(with_attrs1, with_attrs2)
  
  assert_eq(Resource::get_attribute(merged, "string.attr"), Some(StringValue("overridden string")))
  assert_eq(Resource::get_attribute(merged, "int.attr"), Some(IntValue(42)))
  assert_eq(Resource::get_attribute(merged, "float.attr"), Some(FloatValue(3.14159)))
  assert_eq(Resource::get_attribute(merged, "bool.attr"), Some(BoolValue(false)))
  assert_eq(Resource::get_attribute(merged, "array.string"), Some(ArrayStringValue(["item1", "item2"])))
  assert_eq(Resource::get_attribute(merged, "array.int"), Some(ArrayIntValue([1, 2, 3, 4, 5])))
  assert_eq(Resource::get_attribute(merged, "another.int"), Some(IntValue(999)))
  assert_eq(Resource::get_attribute(merged, "another.float"), Some(FloatValue(2.71828)))
}

test "resource empty and null handling" {
  let empty_resource = Resource::new()
  let populated_resource = Resource::new()
  
  // Add attributes to populated resource
  let attrs = [("service.name", StringValue("test-service")), 
                ("empty.string", StringValue(""))]
  let populated = Resource::with_attributes(populated_resource, attrs)
  
  // Merge empty with populated
  let merged1 = Resource::merge(empty_resource, populated)
  assert_eq(Resource::get_attribute(merged1, "service.name"), Some(StringValue("test-service")))
  assert_eq(Resource::get_attribute(merged1, "empty.string"), Some(StringValue("")))
  
  // Merge populated with empty
  let merged2 = Resource::merge(populated, empty_resource)
  assert_eq(Resource::get_attribute(merged2, "service.name"), Some(StringValue("test-service")))
  assert_eq(Resource::get_attribute(merged2, "empty.string"), Some(StringValue("")))
  
  // Merge empty with empty
  let merged3 = Resource::merge(empty_resource, empty_resource)
  assert_eq(Resource::get_attribute(merged3, "service.name"), None)
}

test "resource with special key names" {
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  
  // Keys with special characters
  let attrs1 = [
    ("service.name", StringValue("special-service")),
    ("user.id", StringValue("user-123")),
    ("http-status-code", IntValue(200)),
    ("deployment.region", StringValue("us-west-2"))
  ]
  let with_attrs1 = Resource::with_attributes(resource1, attrs1)
  
  let attrs2 = [
    ("service.name", StringValue("override-special")),
    ("trace.id", StringValue("trace-456")),
    ("span.id", StringValue("span-789"))
  ]
  let with_attrs2 = Resource::with_attributes(resource2, attrs2)
  
  let merged = Resource::merge(with_attrs1, with_attrs2)
  
  assert_eq(Resource::get_attribute(merged, "service.name"), Some(StringValue("override-special")))
  assert_eq(Resource::get_attribute(merged, "user.id"), Some(StringValue("user-123")))
  assert_eq(Resource::get_attribute(merged, "http-status-code"), Some(IntValue(200)))
  assert_eq(Resource::get_attribute(merged, "deployment.region"), Some(StringValue("us-west-2")))
  assert_eq(Resource::get_attribute(merged, "trace.id"), Some(StringValue("trace-456")))
  assert_eq(Resource::get_attribute(merged, "span.id"), Some(StringValue("span-789")))
}