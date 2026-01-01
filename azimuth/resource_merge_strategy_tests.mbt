// Resource Merge Strategy Tests for Azimuth Telemetry System
// This file contains test cases for resource merge strategies

test "resource creation and basic operations" {
  let resource = Resource::new()
  
  // Verify empty resource
  assert_eq(Resource::get_attribute(resource, "any.key"), None)
  
  // Add attributes to resource
  let attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Verify attributes were added
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let service_instance = Resource::get_attribute(resource_with_attrs, "service.instance.id")
  
  assert_eq(service_name, Some(StringValue("test-service")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(service_instance, Some(StringValue("instance-123")))
}

test "resource merge with override strategy" {
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("development"))
  ]
  
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("service.instance.id", StringValue("instance-456")),
    ("host.name", StringValue("localhost"))
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // Merge resources with override strategy
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // Verify override resource takes precedence
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  let service_version = Resource::get_attribute(merged_resource, "service.version")
  let service_instance = Resource::get_attribute(merged_resource, "service.instance.id")
  let deployment_env = Resource::get_attribute(merged_resource, "deployment.environment")
  let host_name = Resource::get_attribute(merged_resource, "host.name")
  
  assert_eq(service_name, Some(StringValue("override-service")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(service_instance, Some(StringValue("instance-456")))
  assert_eq(deployment_env, Some(StringValue("development")))
  assert_eq(host_name, Some(StringValue("localhost")))
}

test "resource merge with different attribute types" {
  let base_attrs = [
    ("service.name", StringValue("multi-type-service")),
    ("process.pid", IntValue(12345)),
    ("process.cpu.percent", FloatValue(75.5)),
    ("service.debug.enabled", BoolValue(true))
  ]
  
  let override_attrs = [
    ("service.name", StringValue("updated-service")),
    ("process.pid", IntValue(67890)),
    ("service.debug.enabled", BoolValue(false)),
    ("service.tags", ArrayStringValue(["tag1", "tag2", "tag3"]))
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // Verify different types are handled correctly
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  let process_pid = Resource::get_attribute(merged_resource, "process.pid")
  let process_cpu = Resource::get_attribute(merged_resource, "process.cpu.percent")
  let debug_enabled = Resource::get_attribute(merged_resource, "service.debug.enabled")
  let service_tags = Resource::get_attribute(merged_resource, "service.tags")
  
  assert_eq(service_name, Some(StringValue("updated-service")))
  assert_eq(process_pid, Some(IntValue(67890)))
  assert_eq(process_cpu, Some(FloatValue(75.5)))
  assert_eq(debug_enabled, Some(BoolValue(false)))
  assert_eq(service_tags, Some(ArrayStringValue(["tag1", "tag2", "tag3"])))
}

test "resource merge with empty resources" {
  let base_attrs = [
    ("service.name", StringValue("base-only-service")),
    ("service.version", StringValue("2.0.0"))
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let empty_resource = Resource::new()
  
  // Merge base with empty
  let merged_with_empty = Resource::merge(base_resource, empty_resource)
  
  // Verify base attributes are preserved
  let service_name = Resource::get_attribute(merged_with_empty, "service.name")
  let service_version = Resource::get_attribute(merged_with_empty, "service.version")
  
  assert_eq(service_name, Some(StringValue("base-only-service")))
  assert_eq(service_version, Some(StringValue("2.0.0")))
  
  // Merge empty with base
  let merged_empty_with = Resource::merge(empty_resource, base_resource)
  
  // Verify base attributes are preserved
  let service_name2 = Resource::get_attribute(merged_empty_with, "service.name")
  let service_version2 = Resource::get_attribute(merged_empty_with, "service.version")
  
  assert_eq(service_name2, Some(StringValue("base-only-service")))
  assert_eq(service_version2, Some(StringValue("2.0.0")))
}

test "resource merge with array attributes" {
  let base_attrs = [
    ("service.name", StringValue("array-service")),
    ("service.endpoints", ArrayStringValue(["http://api1.example.com", "http://api2.example.com"])),
    ("service.ports", ArrayIntValue([8080, 8081]))
  ]
  
  let override_attrs = [
    ("service.name", StringValue("updated-array-service")),
    ("service.endpoints", ArrayStringValue(["http://api3.example.com"])),
    ("service.protocols", ArrayStringValue(["http", "https"]))
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // Verify array attributes are handled correctly
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  let endpoints = Resource::get_attribute(merged_resource, "service.endpoints")
  let ports = Resource::get_attribute(merged_resource, "service.ports")
  let protocols = Resource::get_attribute(merged_resource, "service.protocols")
  
  assert_eq(service_name, Some(StringValue("updated-array-service")))
  assert_eq(endpoints, Some(ArrayStringValue(["http://api3.example.com"])))
  assert_eq(ports, Some(ArrayIntValue([8080, 8081])))
  assert_eq(protocols, Some(ArrayStringValue(["http", "https"])))
}

test "resource merge with special characters and unicode" {
  let base_attrs = [
    ("service.name", StringValue("特殊服务")),
    ("service.description", StringValue("Service with special chars: @#$%^&*()")),
    ("service.unicode.tags", StringValue("标签1,标签2,标签3"))
  ]
  
  let override_attrs = [
    ("service.name", StringValue("更新服务")),
    ("service.encoding", StringValue("UTF-8")),
    ("service.emoji", StringValue("🚀📊📈"))
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // Verify unicode and special characters are preserved
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  let service_desc = Resource::get_attribute(merged_resource, "service.description")
  let unicode_tags = Resource::get_attribute(merged_resource, "service.unicode.tags")
  let encoding = Resource::get_attribute(merged_resource, "service.encoding")
  let emoji = Resource::get_attribute(merged_resource, "service.emoji")
  
  assert_eq(service_name, Some(StringValue("更新服务")))
  assert_eq(service_desc, Some(StringValue("Service with special chars: @#$%^&*()")))
  assert_eq(unicode_tags, Some(StringValue("标签1,标签2,标签3")))
  assert_eq(encoding, Some(StringValue("UTF-8")))
  assert_eq(emoji, Some(StringValue("🚀📊📈")))
}

test "resource merge chain operations" {
  // Create multiple resources to merge in a chain
  let resource1 = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("service-1")),
    ("service.version", StringValue("1.0.0"))
  ])
  
  let resource2 = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("service-2")),
    ("deployment.environment", StringValue("production"))
  ])
  
  let resource3 = Resource::with_attributes(Resource::new(), [
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("prod-server-01"))
  ])
  
  // Merge in chain: resource1 -> resource2 -> resource3
  let merged1_2 = Resource::merge(resource1, resource2)
  let final_merged = Resource::merge(merged1_2, resource3)
  
  // Verify final merged resource contains all expected attributes
  let service_name = Resource::get_attribute(final_merged, "service.name")
  let service_version = Resource::get_attribute(final_merged, "service.version")
  let deployment_env = Resource::get_attribute(final_merged, "deployment.environment")
  let instance_id = Resource::get_attribute(final_merged, "service.instance.id")
  let host_name = Resource::get_attribute(final_merged, "host.name")
  
  assert_eq(service_name, Some(StringValue("service-2")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(deployment_env, Some(StringValue("production")))
  assert_eq(instance_id, Some(StringValue("instance-123")))
  assert_eq(host_name, Some(StringValue("prod-server-01")))
}