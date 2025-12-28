// Test file for basic telemetry types and attributes
test "attribute value type conversions" {
  // Test string attribute
  let string_attr = StringValue("test_value")
  match string_attr {
    StringValue(s) => assert_eq(s, "test_value")
    _ => assert_true(false, "Expected StringValue")
  }
  
  // Test int attribute
  let int_attr = IntValue(42)
  match int_attr {
    IntValue(i) => assert_eq(i, 42)
    _ => assert_true(false, "Expected IntValue")
  }
  
  // Test float attribute
  let float_attr = FloatValue(3.14)
  match float_attr {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => assert_true(false, "Expected FloatValue")
  }
  
  // Test bool attribute
  let bool_attr = BoolValue(true)
  match bool_attr {
    BoolValue(b) => assert_eq(b, true)
    _ => assert_true(false, "Expected BoolValue")
  }
  
  // Test array string attribute
  let array_str_attr = ArrayStringValue(["a", "b", "c"])
  match array_str_attr {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => assert_true(false, "Expected ArrayStringValue")
  }
  
  // Test array int attribute
  let array_int_attr = ArrayIntValue([1, 2, 3])
  match array_int_attr {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1)
      assert_eq(arr[1], 2)
      assert_eq(arr[2], 3)
    }
    _ => assert_true(false, "Expected ArrayIntValue")
  }
}

test "attributes operations" {
  let attrs = Attributes::new()
  
  // Test setting and getting string attribute
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  let result = Attributes::get(attrs, "string.key")
  assert_true(result.is_some(), "Should find string key")
  match result {
    Some(StringValue(s)) => assert_eq(s, "test_value")
    _ => assert_true(false, "Expected StringValue")
  }
  
  // Test setting and getting int attribute
  Attributes::set(attrs, "int.key", IntValue(42))
  let int_result = Attributes::get(attrs, "int.key")
  assert_true(int_result.is_some(), "Should find int key")
  match int_result {
    Some(IntValue(i)) => assert_eq(i, 42)
    _ => assert_true(false, "Expected IntValue")
  }
  
  // Test getting non-existent key
  let none_result = Attributes::get(attrs, "non.existent.key")
  assert_true(none_result.is_none(), "Should not find non-existent key")
}

test "resource operations" {
  let resource = Resource::new()
  
  // Test resource with attributes
  let attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Test getting attributes
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  assert_true(service_name.is_some(), "Should find service name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_true(false, "Expected StringValue for service name")
  }
  
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  assert_true(service_version.is_some(), "Should find service version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false, "Expected StringValue for service version")
  }
  
  // Test merging resources
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("environment", StringValue("test"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged = Resource::merge(resource_with_attrs, override_resource)
  
  let merged_name = Resource::get_attribute(merged, "service.name")
  match merged_name {
    Some(StringValue(name)) => assert_eq(name, "override-service")
    _ => assert_true(false, "Expected overridden service name")
  }
  
  let environment = Resource::get_attribute(merged, "environment")
  match environment {
    Some(StringValue(env)) => assert_eq(env, "test")
    _ => assert_true(false, "Expected environment attribute")
  }
}

test "instrumentation scope operations" {
  // Test creating instrumentation scope
  let scope = InstrumentationScope::{
    name: "test-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  
  assert_eq(scope.name, "test-scope")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, Some("https://example.com/schema"))
  
  // Test scope with optional fields
  let minimal_scope = InstrumentationScope::{
    name: "minimal-scope",
    version: None,
    schema_url: None
  }
  
  assert_eq(minimal_scope.name, "minimal-scope")
  assert_true(minimal_scope.version.is_none())
  assert_true(minimal_scope.schema_url.is_none())
}