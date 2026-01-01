// Resource Management Test Suite for Azimuth Telemetry System
// This file contains test cases for Resource operations

test "resource creation and basic operations" {
  let resource = Resource::new()
  
  // Test creating resource with attributes
  let attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  // Verify attributes can be retrieved
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  assert_eq(service_name, Some(StringValue("azimuth-service")))
  
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  assert_eq(service_version, Some(StringValue("1.0.0")))
  
  let service_instance_id = Resource::get_attribute(resource_with_attrs, "service.instance.id")
  assert_eq(service_instance_id, Some(StringValue("instance-123")))
  
  // Test getting non-existent attribute
  let missing_attribute = Resource::get_attribute(resource_with_attrs, "missing.attribute")
  assert_eq(missing_attribute, None)
}

test "resource with different attribute types" {
  let resource = Resource::new()
  
  // Test with different attribute value types
  let attributes = [
    ("string.attr", StringValue("test-string")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("array.string.attr", ArrayStringValue(["item1", "item2", "item3"])),
    ("array.int.attr", ArrayIntValue([10, 20, 30]))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  // Verify string attribute
  let string_result = Resource::get_attribute(resource_with_attrs, "string.attr")
  match string_result {
    Some(StringValue(value)) => assert_eq(value, "test-string")
    _ => assert_true(false, "Expected StringValue")
  }
  
  // Verify int attribute
  let int_result = Resource::get_attribute(resource_with_attrs, "int.attr")
  match int_result {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false, "Expected IntValue")
  }
  
  // Verify float attribute
  let float_result = Resource::get_attribute(resource_with_attrs, "float.attr")
  match float_result {
    Some(FloatValue(value)) => assert_eq(value, 3.14159)
    _ => assert_true(false, "Expected FloatValue")
  }
  
  // Verify bool attribute
  let bool_result = Resource::get_attribute(resource_with_attrs, "bool.attr")
  match bool_result {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false, "Expected BoolValue")
  }
  
  // Verify array string attribute
  let array_string_result = Resource::get_attribute(resource_with_attrs, "array.string.attr")
  match array_string_result {
    Some(ArrayStringValue(values)) => assert_eq(values, ["item1", "item2", "item3"])
    _ => assert_true(false, "Expected ArrayStringValue")
  }
  
  // Verify array int attribute
  let array_int_result = Resource::get_attribute(resource_with_attrs, "array.int.attr")
  match array_int_result {
    Some(ArrayIntValue(values)) => assert_eq(values, [10, 20, 30])
    _ => assert_true(false, "Expected ArrayIntValue")
  }
}

test "resource merge operations" {
  // Create base resource
  let base_resource = Resource::new()
  let base_attributes = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("common.attr", StringValue("base-value"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attributes)
  
  // Create override resource
  let override_resource = Resource::new()
  let override_attributes = [
    ("service.name", StringValue("override-service")),  // This should override
    ("service.environment", StringValue("production")), // This should be added
    ("common.attr", StringValue("override-value"))      // This should override
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attributes)
  
  // Merge resources
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify merged attributes
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  assert_eq(service_name, Some(StringValue("override-service")))
  
  let service_version = Resource::get_attribute(merged_resource, "service.version")
  assert_eq(service_version, Some(StringValue("1.0.0")))
  
  let service_environment = Resource::get_attribute(merged_resource, "service.environment")
  assert_eq(service_environment, Some(StringValue("production")))
  
  let common_attr = Resource::get_attribute(merged_resource, "common.attr")
  assert_eq(common_attr, Some(StringValue("override-value")))
}

test "resource with service telemetry attributes" {
  let resource = Resource::new()
  
  // Test with typical OpenTelemetry resource attributes
  let telemetry_attributes = [
    ("service.name", StringValue("azimuth-telemetry")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("azimuth-instance-456")),
    ("service.namespace", StringValue("telemetry")),
    ("deployment.environment", StringValue("staging")),
    ("host.name", StringValue("azimuth-host")),
    ("host.arch", StringValue("amd64")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("5.15.0")),
    ("process.pid", IntValue(12345)),
    ("process.executable.name", StringValue("azimuth")),
    ("process.command_args", ArrayStringValue(["azimuth", "--config", "/etc/azimuth/config.yaml"]))
  ]
  let telemetry_resource = Resource::with_attributes(resource, telemetry_attributes)
  
  // Verify service attributes
  assert_eq(
    Resource::get_attribute(telemetry_resource, "service.name"),
    Some(StringValue("azimuth-telemetry"))
  )
  assert_eq(
    Resource::get_attribute(telemetry_resource, "service.version"),
    Some(StringValue("2.1.0"))
  )
  assert_eq(
    Resource::get_attribute(telemetry_resource, "service.instance.id"),
    Some(StringValue("azimuth-instance-456"))
  )
  
  // Verify deployment attributes
  assert_eq(
    Resource::get_attribute(telemetry_resource, "deployment.environment"),
    Some(StringValue("staging"))
  )
  
  // Verify host attributes
  assert_eq(
    Resource::get_attribute(telemetry_resource, "host.name"),
    Some(StringValue("azimuth-host"))
  )
  assert_eq(
    Resource::get_attribute(telemetry_resource, "host.arch"),
    Some(StringValue("amd64"))
  )
  
  // Verify OS attributes
  assert_eq(
    Resource::get_attribute(telemetry_resource, "os.type"),
    Some(StringValue("linux"))
  )
  assert_eq(
    Resource::get_attribute(telemetry_resource, "os.version"),
    Some(StringValue("5.15.0"))
  )
  
  // Verify process attributes
  let process_pid = Resource::get_attribute(telemetry_resource, "process.pid")
  match process_pid {
    Some(IntValue(pid)) => assert_eq(pid, 12345)
    _ => assert_true(false, "Expected IntValue")
  }
  
  assert_eq(
    Resource::get_attribute(telemetry_resource, "process.executable.name"),
    Some(StringValue("azimuth"))
  )
  
  let process_command_args = Resource::get_attribute(telemetry_resource, "process.command_args")
  match process_command_args {
    Some(ArrayStringValue(args)) => assert_eq(args, ["azimuth", "--config", "/etc/azimuth/config.yaml"])
    _ => assert_true(false, "Expected ArrayStringValue")
  }
}