// Attributes Operations Test Cases for Azimuth Telemetry System
// Testing attribute value operations and type conversions

test "attribute creation and basic operations" {
  let attrs = Attributes::new()
  
  // Test setting string attribute
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  let string_value = Attributes::get(attrs, "string.key")
  assert_eq(string_value, Some(StringValue("test_value")))
  
  // Test setting int attribute
  Attributes::set(attrs, "int.key", IntValue(42))
  let int_value = Attributes::get(attrs, "int.key")
  assert_eq(int_value, Some(IntValue(42)))
  
  // Test getting non-existent attribute
  let missing_value = Attributes::get(attrs, "missing.key")
  assert_eq(missing_value, None)
}

test "attribute value type handling" {
  let string_val = StringValue("hello")
  let int_val = IntValue(123)
  let float_val = FloatValue(3.14)
  let bool_val = BoolValue(true)
  
  // Test string array value
  let string_array_val = ArrayStringValue(["a", "b", "c"])
  
  // Test int array value
  let int_array_val = ArrayIntValue([1, 2, 3])
  
  // Verify values are correctly constructed
  match string_val {
    StringValue(s) => assert_eq(s, "hello")
    _ => assert_true(false)
  }
  
  match int_val {
    IntValue(i) => assert_eq(i, 123)
    _ => assert_true(false)
  }
  
  match float_val {
    FloatValue(f) => assert_true(f > 3.0 && f < 3.2)
    _ => assert_true(false)
  }
  
  match bool_val {
    BoolValue(b) => assert_true(b)
    _ => assert_true(false)
  }
  
  match string_array_val {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => assert_true(false)
  }
  
  match int_array_val {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1)
      assert_eq(arr[1], 2)
      assert_eq(arr[2], 3)
    }
    _ => assert_true(false)
  }
}

test "resource with attributes" {
  let resource = Resource::new()
  
  // Test resource with no attributes
  assert_eq(resource.attributes.length(), 0)
  
  // Add attributes to resource
  let attrs = [("service.name", StringValue("test-service")), 
               ("service.version", StringValue("1.0.0")),
               ("service.instance.id", StringValue("instance-123"))]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  assert_eq(resource_with_attrs.attributes.length(), 3)
  
  // Test getting resource attribute
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_true(false)
  }
  
  // Test getting missing attribute
  let missing_attr = Resource::get_attribute(resource_with_attrs, "missing.key")
  assert_eq(missing_attr, None)
}

test "instrumentation scope attributes" {
  let scope = InstrumentationScope::{ 
    name: "test-scope", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/schema") 
  }
  
  assert_eq(scope.name, "test-scope")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, Some("https://example.com/schema"))
  
  // Test scope with minimal attributes
  let minimal_scope = InstrumentationScope::{ 
    name: "minimal-scope", 
    version: None, 
    schema_url: None 
  }
  
  assert_eq(minimal_scope.name, "minimal-scope")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
}