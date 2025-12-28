// Resource Attributes Operations Test Suite
// Tests for Resource creation, attribute management, and resource operations

test "resource_basic_creation" {
  // Test basic Resource creation
  let resource = Resource::new()
  assert_eq(resource.attributes.length(), 0)
}

test "resource_with_attributes" {
  // Test Resource creation with attributes
  let attributes = [
    ("service.name", StringValue("my-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attributes)
  assert_eq(resource.attributes.length(), 3)
}

test "resource_attribute_retrieval" {
  // Test retrieving attributes from resource
  let attributes = [
    ("service.name", StringValue("my-service")),
    ("host.name", StringValue("localhost")),
    ("process.pid", IntValue(1234)),
    ("process.start.time", StringValue("2023-01-01T00:00:00Z"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attributes)
  
  // Test string attribute retrieval
  match Resource::get_attribute(resource, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "my-service")
    _ => assert_true(false, "Expected StringValue for service.name")
  }
  
  // Test int attribute retrieval
  match Resource::get_attribute(resource, "process.pid") {
    Some(IntValue(pid)) => assert_eq(pid, 1234)
    _ => assert_true(false, "Expected IntValue for process.pid")
  }
  
  // Test non-existent attribute
  match Resource::get_attribute(resource, "non.existent") {
    None => assert_true(true)
    Some(_) => assert_true(false, "Expected None for non-existent attribute")
  }
}

test "resource_merge_operations" {
  // Test merging two resources
  let base_attributes = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("development"))
  ]
  
  let override_attributes = [
    ("service.name", StringValue("override-service")),
    ("host.name", StringValue("prod-server")),
    ("deployment.environment", StringValue("production"))
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attributes)
  let override_resource = Resource::with_attributes(Resource::new(), override_attributes)
  
  // Merge resources (simplified implementation returns override)
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // In simplified implementation, merged should equal override
  match Resource::get_attribute(merged_resource, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "override-service")
    _ => assert_true(false, "Expected overridden service name")
  }
  
  match Resource::get_attribute(merged_resource, "host.name") {
    Some(StringValue(host)) => assert_eq(host, "prod-server")
    _ => assert_true(false, "Expected host name from override")
  }
}

test "resource_different_attribute_types" {
  // Test resource with different attribute value types
  let attributes = [
    ("string.attr", StringValue("string value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("array.string.attr", ArrayStringValue(["item1", "item2", "item3"])),
    ("array.int.attr", ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attributes)
  
  // Test string attribute
  match Resource::get_attribute(resource, "string.attr") {
    Some(StringValue(value)) => assert_eq(value, "string value")
    _ => assert_true(false, "Expected StringValue")
  }
  
  // Test int attribute
  match Resource::get_attribute(resource, "int.attr") {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false, "Expected IntValue")
  }
  
  // Test float attribute
  match Resource::get_attribute(resource, "float.attr") {
    Some(FloatValue(value)) => assert_eq(value, 3.14159)
    _ => assert_true(false, "Expected FloatValue")
  }
  
  // Test bool attribute
  match Resource::get_attribute(resource, "bool.attr") {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false, "Expected BoolValue")
  }
  
  // Test array string attribute
  match Resource::get_attribute(resource, "array.string.attr") {
    Some(ArrayStringValue(values)) => assert_eq(values.length(), 3)
    _ => assert_true(false, "Expected ArrayStringValue")
  }
  
  // Test array int attribute
  match Resource::get_attribute(resource, "array.int.attr") {
    Some(ArrayIntValue(values)) => assert_eq(values.length(), 5)
    _ => assert_true(false, "Expected ArrayIntValue")
  }
}

test "resource_empty_attributes_handling" {
  // Test resource with empty and null-like attributes
  let attributes = [
    ("empty.string", StringValue("")),
    ("zero.int", IntValue(0)),
    ("zero.float", FloatValue(0.0)),
    ("false.bool", BoolValue(false))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attributes)
  
  // Test empty string
  match Resource::get_attribute(resource, "empty.string") {
    Some(StringValue(value)) => {
      assert_eq(value, "")
      assert_eq(value.length(), 0)
    }
    _ => assert_true(false, "Expected empty string")
  }
  
  // Test zero values
  match Resource::get_attribute(resource, "zero.int") {
    Some(IntValue(value)) => assert_eq(value, 0)
    _ => assert_true(false, "Expected zero int")
  }
  
  match Resource::get_attribute(resource, "zero.float") {
    Some(FloatValue(value)) => assert_eq(value, 0.0)
    _ => assert_true(false, "Expected zero float")
  }
  
  match Resource::get_attribute(resource, "false.bool") {
    Some(BoolValue(value)) => assert_eq(value, false)
    _ => assert_true(false, "Expected false bool")
  }
}

test "resource_special_characters_in_attributes" {
  // Test resource attributes with special characters
  let attributes = [
    ("service.name", StringValue("my-service")),
    ("host.name", StringValue("server-01.example.com")),
    ("user.email", StringValue("test@example.com")),
    ("unicode.attr", StringValue("用户属性")),
    ("json.data", StringValue("{\"key\": \"value\", \"number\": 123}")),
    ("url.attr", StringValue("https://example.com/path?query=value&other=data"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attributes)
  
  // Test email attribute
  match Resource::get_attribute(resource, "user.email") {
    Some(StringValue(email)) => assert_eq(email, "test@example.com")
    _ => assert_true(false, "Expected email attribute")
  }
  
  // Test unicode attribute
  match Resource::get_attribute(resource, "unicode.attr") {
    Some(StringValue(value)) => assert_eq(value, "用户属性")
    _ => assert_true(false, "Expected unicode attribute")
  }
  
  // Test JSON attribute
  match Resource::get_attribute(resource, "json.data") {
    Some(StringValue(json)) => assert_eq(json, "{\"key\": \"value\", \"number\": 123}")
    _ => assert_true(false, "Expected JSON attribute")
  }
  
  // Test URL attribute
  match Resource::get_attribute(resource, "url.attr") {
    Some(StringValue(url)) => assert_eq(url, "https://example.com/path?query=value&other=data")
    _ => assert_true(false, "Expected URL attribute")
  }
}

test "resource_long_attribute_values" {
  // Test resource with long attribute values
  let long_description = "This is a very long description that might be used " +
                         "to store detailed information about the service, " +
                         "including its purpose, configuration details, and " +
                         "other metadata that helps with monitoring and " +
                         "debugging in distributed systems."
  
  let attributes = [
    ("service.description", StringValue(long_description)),
    ("service.tags", ArrayStringValue(["tag1", "tag2", "tag3", "very-long-tag-name-that-exceeds-normal-length"]))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attributes)
  
  // Test long string attribute
  match Resource::get_attribute(resource, "service.description") {
    Some(StringValue(desc)) => {
      assert_eq(desc, long_description)
      assert_true(desc.length() > 200)
    }
    _ => assert_true(false, "Expected long description")
  }
  
  // Test array with long tag
  match Resource::get_attribute(resource, "service.tags") {
    Some(ArrayStringValue(tags)) => {
      assert_eq(tags.length(), 4)
      assert_true(tags[3].length() > 40)
    }
    _ => assert_true(false, "Expected array string attribute")
  }
}

test "resource_case_sensitivity" {
  // Test that resource attribute keys are case sensitive
  let attributes = [
    ("Service.Name", StringValue("uppercase")),
    ("service.name", StringValue("lowercase")),
    ("SERVICE.NAME", StringValue("all caps"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attributes)
  
  // Each key should be distinct
  match Resource::get_attribute(resource, "Service.Name") {
    Some(StringValue(value)) => assert_eq(value, "uppercase")
    _ => assert_true(false, "Expected uppercase service name")
  }
  
  match Resource::get_attribute(resource, "service.name") {
    Some(StringValue(value)) => assert_eq(value, "lowercase")
    _ => assert_true(false, "Expected lowercase service name")
  }
  
  match Resource::get_attribute(resource, "SERVICE.NAME") {
    Some(StringValue(value)) => assert_eq(value, "all caps")
    _ => assert_true(false, "Expected all caps service name")
  }
}

test "resource_empty_and_single_attribute" {
  // Test resource with no attributes and single attribute
  let empty_resource = Resource::new()
  assert_eq(empty_resource.attributes.length(), 0)
  
  let single_attributes = [("only.attr", StringValue("only value"))]
  let single_resource = Resource::with_attributes(empty_resource, single_attributes)
  assert_eq(single_resource.attributes.length(), 1)
  
  match Resource::get_attribute(single_resource, "only.attr") {
    Some(StringValue(value)) => assert_eq(value, "only value")
    _ => assert_true(false, "Expected single attribute value")
  }
}