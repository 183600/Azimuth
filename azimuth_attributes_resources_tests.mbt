// Azimuth Telemetry System - Attributes and Resources Test Suite
// This file contains test cases for attributes and resources functionality

// Test 1: Attributes Basic Operations
test "attributes basic operations" {
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

// Test 2: Attribute Value Type Conversions
test "attribute value type conversions" {
  // Test string attribute value
  let string_attr = StringValue("hello world")
  match string_attr {
    StringValue(s) => assert_eq(s, "hello world")
    _ => assert_true(false) // Should not reach here
  }
  
  // Test int attribute value
  let int_attr = IntValue(123)
  match int_attr {
    IntValue(i) => assert_eq(i, 123)
    _ => assert_true(false) // Should not reach here
  }
  
  // Test float attribute value
  let float_attr = FloatValue(3.14)
  match float_attr {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => assert_true(false) // Should not reach here
  }
  
  // Test bool attribute value
  let bool_attr = BoolValue(true)
  match bool_attr {
    BoolValue(b) => assert_eq(b, true)
    _ => assert_true(false) // Should not reach here
  }
  
  // Test array string attribute value
  let array_string_attr = ArrayStringValue(["a", "b", "c"])
  match array_string_attr {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => assert_true(false) // Should not reach here
  }
  
  // Test array int attribute value
  let array_int_attr = ArrayIntValue([1, 2, 3, 4, 5])
  match array_int_attr {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
    }
    _ => assert_true(false) // Should not reach here
  }
}

// Test 3: Resource Basic Operations
test "resource basic operations" {
  let resource = Resource::new()
  
  // Test creating resource with attributes
  let attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  // Test getting attributes
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  assert_eq(service_name, Some(StringValue("azimuth-service")))
  
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  assert_eq(service_version, Some(StringValue("1.0.0")))
  
  let service_instance_id = Resource::get_attribute(resource_with_attrs, "service.instance.id")
  assert_eq(service_instance_id, Some(StringValue("instance-123")))
  
  // Test getting non-existent attribute
  let missing_attr = Resource::get_attribute(resource_with_attrs, "missing.attribute")
  assert_eq(missing_attr, None)
}

// Test 4: Resource Merge Operations
test "resource merge operations" {
  // Create base resource
  let base_attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production"))
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  // Create override resource
  let override_attrs = [
    ("service.version", StringValue("2.0.0")), // This should override
    ("host.name", StringValue("web-server-01")),
    ("host.ip", StringValue("192.168.1.100"))
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // Merge resources
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // Verify merged attributes
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  assert_eq(service_name, Some(StringValue("azimuth-service")))
  
  let service_version = Resource::get_attribute(merged_resource, "service.version")
  assert_eq(service_version, Some(StringValue("2.0.0"))) // Should be overridden
  
  let deployment_env = Resource::get_attribute(merged_resource, "deployment.environment")
  assert_eq(deployment_env, Some(StringValue("production")))
  
  let host_name = Resource::get_attribute(merged_resource, "host.name")
  assert_eq(host_name, Some(StringValue("web-server-01")))
  
  let host_ip = Resource::get_attribute(merged_resource, "host.ip")
  assert_eq(host_ip, Some(StringValue("192.168.1.100")))
}

// Test 5: Complex Attribute Operations
test "complex attribute operations" {
  let attrs = Attributes::new()
  
  // Test setting various attribute types
  Attributes::set(attrs, "string.attr", StringValue("test string"))
  Attributes::set(attrs, "int.attr", IntValue(42))
  Attributes::set(attrs, "float.attr", FloatValue(3.14159))
  Attributes::set(attrs, "bool.attr", BoolValue(true))
  Attributes::set(attrs, "array.string.attr", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "array.int.attr", ArrayIntValue([1, 2, 3]))
  
  // Test retrieving and verifying each attribute type
  let string_attr = Attributes::get(attrs, "string.attr")
  match string_attr {
    Some(StringValue(s)) => assert_eq(s, "test string")
    _ => assert_true(false)
  }
  
  let int_attr = Attributes::get(attrs, "int.attr")
  match int_attr {
    Some(IntValue(i)) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  let float_attr = Attributes::get(attrs, "float.attr")
  match float_attr {
    Some(FloatValue(f)) => assert_true(f > 3.14 && f < 3.15)
    _ => assert_true(false)
  }
  
  let bool_attr = Attributes::get(attrs, "bool.attr")
  match bool_attr {
    Some(BoolValue(b)) => assert_eq(b, true)
    _ => assert_true(false)
  }
  
  let array_string_attr = Attributes::get(attrs, "array.string.attr")
  match array_string_attr {
    Some(ArrayStringValue(arr)) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => assert_true(false)
  }
  
  let array_int_attr = Attributes::get(attrs, "array.int.attr")
  match array_int_attr {
    Some(ArrayIntValue(arr)) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1)
      assert_eq(arr[1], 2)
      assert_eq(arr[2], 3)
    }
    _ => assert_true(false)
  }
}

// Test 6: Resource with Complex Attributes
test "resource with complex attributes" {
  // Create complex attributes
  let complex_attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.tags", ArrayStringValue(["web", "api", "public"])),
    ("service.ports", ArrayIntValue([8080, 8443])),
    ("service.enabled", BoolValue(true)),
    ("service.weight", FloatValue(0.75)),
    ("service.instances", IntValue(3))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), complex_attrs)
  
  // Test retrieving complex attributes
  let service_name = Resource::get_attribute(resource, "service.name")
  assert_eq(service_name, Some(StringValue("azimuth-service")))
  
  let service_tags = Resource::get_attribute(resource, "service.tags")
  match service_tags {
    Some(ArrayStringValue(tags)) => {
      assert_eq(tags.length(), 3)
      assert_true(tags.contains("web"))
      assert_true(tags.contains("api"))
      assert_true(tags.contains("public"))
    }
    _ => assert_true(false)
  }
  
  let service_ports = Resource::get_attribute(resource, "service.ports")
  match service_ports {
    Some(ArrayIntValue(ports)) => {
      assert_eq(ports.length(), 2)
      assert_true(ports.contains(8080))
      assert_true(ports.contains(8443))
    }
    _ => assert_true(false)
  }
  
  let service_enabled = Resource::get_attribute(resource, "service.enabled")
  match service_enabled {
    Some(BoolValue(enabled)) => assert_eq(enabled, true)
    _ => assert_true(false)
  }
  
  let service_weight = Resource::get_attribute(resource, "service.weight")
  match service_weight {
    Some(FloatValue(weight)) => assert_eq(weight, 0.75)
    _ => assert_true(false)
  }
  
  let service_instances = Resource::get_attribute(resource, "service.instances")
  match service_instances {
    Some(IntValue(instances)) => assert_eq(instances, 3)
    _ => assert_true(false)
  }
}