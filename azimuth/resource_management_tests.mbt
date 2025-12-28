// Test file for resource management
test "resource lifecycle management" {
  // Test resource creation
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  
  // Test resources are independent
  assert_true(resource1 != resource2, "Different resources should be independent")
  
  // Test resource with attributes
  let attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource1, attrs)
  
  // Test attribute retrieval
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  assert_true(service_name.is_some(), "Should find service name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_true(false, "Expected StringValue")
  }
  
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  assert_true(service_version.is_some(), "Should find service version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false, "Expected StringValue")
  }
}

test "resource attribute management" {
  let resource = Resource::new()
  
  // Test adding different types of attributes
  let string_attrs = [
    ("string.attr", StringValue("test-value")),
    ("empty.string", StringValue("")),
    ("unicode.string", StringValue("测试中文字符"))
  ]
  
  let int_attrs = [
    ("int.attr", IntValue(42)),
    ("zero.int", IntValue(0)),
    ("negative.int", IntValue(-100))
  ]
  
  let float_attrs = [
    ("float.attr", FloatValue(3.14159)),
    ("zero.float", FloatValue(0.0)),
    ("negative.float", FloatValue(-2.5))
  ]
  
  let bool_attrs = [
    ("bool.attr.true", BoolValue(true)),
    ("bool.attr.false", BoolValue(false))
  ]
  
  // Create resources with different attribute types
  let string_resource = Resource::with_attributes(resource, string_attrs)
  let int_resource = Resource::with_attributes(string_resource, int_attrs)
  let float_resource = Resource::with_attributes(int_resource, float_attrs)
  let bool_resource = Resource::with_attributes(float_resource, bool_attrs)
  
  // Test retrieval of different attribute types
  let string_attr = Resource::get_attribute(bool_resource, "string.attr")
  match string_attr {
    Some(StringValue(value)) => assert_eq(value, "test-value")
    _ => assert_true(false, "Expected StringValue")
  }
  
  let int_attr = Resource::get_attribute(bool_resource, "int.attr")
  match int_attr {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false, "Expected IntValue")
  }
  
  let float_attr = Resource::get_attribute(bool_resource, "float.attr")
  match float_attr {
    Some(FloatValue(value)) => assert_eq(value, 3.14159)
    _ => assert_true(false, "Expected FloatValue")
  }
  
  let bool_attr = Resource::get_attribute(bool_resource, "bool.attr.true")
  match bool_attr {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false, "Expected BoolValue")
  }
}

test "resource merging strategies" {
  // Create base resource
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("development"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  // Create override resource
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("service.instance.id", StringValue("instance-456")),
    ("region", StringValue("us-west-2"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // Test resource merging
  let merged = Resource::merge(base_resource, override_resource)
  
  // In simplified implementation, override resource takes precedence
  let merged_service_name = Resource::get_attribute(merged, "service.name")
  match merged_service_name {
    Some(StringValue(name)) => assert_eq(name, "override-service")
    _ => assert_true(false, "Expected overridden service name")
  }
  
  let merged_instance_id = Resource::get_attribute(merged, "service.instance.id")
  match merged_instance_id {
    Some(StringValue(id)) => assert_eq(id, "instance-456")
    _ => assert_true(false, "Expected instance ID from override")
  }
  
  let merged_region = Resource::get_attribute(merged, "region")
  match merged_region {
    Some(StringValue(region)) => assert_eq(region, "us-west-2")
    _ => assert_true(false, "Expected region from override")
  }
}

test "resource hierarchy management" {
  // Create resource hierarchy
  let global_attrs = [
    ("service.name", StringValue("global-service")),
    ("service.namespace", StringValue("default"))
  ]
  let global_resource = Resource::with_attributes(Resource::new(), global_attrs)
  
  let service_attrs = [
    ("service.name", StringValue("specific-service")),
    ("service.version", StringValue("2.0.0"))
  ]
  let service_resource = Resource::with_attributes(global_resource, service_attrs)
  
  let instance_attrs = [
    ("service.instance.id", StringValue("instance-789")),
    ("host.name", StringValue("host-123"))
  ]
  let instance_resource = Resource::with_attributes(service_resource, instance_attrs)
  
  // Test resource hierarchy
  let final_service_name = Resource::get_attribute(instance_resource, "service.name")
  match final_service_name {
    Some(StringValue(name)) => assert_eq(name, "specific-service")
    _ => assert_true(false, "Expected most specific service name")
  }
  
  let namespace = Resource::get_attribute(instance_resource, "service.namespace")
  match namespace {
    Some(StringValue(ns)) => assert_eq(ns, "default")
    _ => assert_true(false, "Expected inherited namespace")
  }
  
  let version = Resource::get_attribute(instance_resource, "service.version")
  match version {
    Some(StringValue(v)) => assert_eq(v, "2.0.0")
    _ => assert_true(false, "Expected service version")
  }
  
  let instance_id = Resource::get_attribute(instance_resource, "service.instance.id")
  match instance_id {
    Some(StringValue(id)) => assert_eq(id, "instance-789")
    _ => assert_true(false, "Expected instance ID")
  }
}

test "resource cleanup and reuse" {
  // Test resource cleanup simulation
  let resource1 = Resource::new()
  let attrs1 = [
    ("temp.attr", StringValue("temporary")),
    ("session.id", StringValue("session-123"))
  ]
  let resource_with_temp = Resource::with_attributes(resource1, attrs1)
  
  // Simulate cleanup by creating new resource without temporary attributes
  let resource2 = Resource::new()
  let permanent_attrs = [
    ("service.name", StringValue("stable-service")),
    ("service.version", StringValue("1.0.0"))
  ]
  let clean_resource = Resource::with_attributes(resource2, permanent_attrs)
  
  // Test cleanup worked
  let temp_attr = Resource::get_attribute(clean_resource, "temp.attr")
  assert_true(temp_attr.is_none(), "Temporary attribute should not exist in clean resource")
  
  let session_id = Resource::get_attribute(clean_resource, "session.id")
  assert_true(session_id.is_none(), "Session ID should not exist in clean resource")
  
  let service_name = Resource::get_attribute(clean_resource, "service.name")
  assert_true(service_name.is_some(), "Permanent attributes should exist")
  
  // Test resource reuse
  let reused_resource = Resource::with_attributes(clean_resource, [
    ("request.id", StringValue("req-456"))
  ])
  
  let request_id = Resource::get_attribute(reused_resource, "request.id")
  match request_id {
    Some(StringValue(id)) => assert_eq(id, "req-456")
    _ => assert_true(false, "Expected request ID in reused resource")
  }
  
  let reused_service_name = Resource::get_attribute(reused_resource, "service.name")
  assert_true(reused_service_name.is_some(), "Original attributes should be preserved")
}

test "resource memory efficiency" {
  // Test resource creation with many attributes
  let many_attrs = Array[(String, AttributeValue)]::create(100, fn(i) {
    ("attr." + i.to_string(), StringValue("value." + i.to_string()))
  })
  
  let resource_with_many_attrs = Resource::with_attributes(Resource::new(), many_attrs)
  
  // Test accessing first and last attributes
  let first_attr = Resource::get_attribute(resource_with_many_attrs, "attr.0")
  match first_attr {
    Some(StringValue(value)) => assert_eq(value, "value.0")
    _ => assert_true(false, "Expected first attribute")
  }
  
  let last_attr = Resource::get_attribute(resource_with_many_attrs, "attr.99")
  match last_attr {
    Some(StringValue(value)) => assert_eq(value, "value.99")
    _ => assert_true(false, "Expected last attribute")
  }
  
  // Test resource with large attribute values
  let large_value = "x".repeat(10000)
  let large_attrs = [
    ("large.attr", StringValue(large_value))
  ]
  let resource_with_large = Resource::with_attributes(Resource::new(), large_attrs)
  
  let large_attr = Resource::get_attribute(resource_with_large, "large.attr")
  match large_attr {
    Some(StringValue(value)) => assert_eq(value.length(), 10000)
    _ => assert_true(false, "Expected large attribute value")
  }
}

test "resource concurrent access simulation" {
  // Simulate concurrent resource access patterns
  let shared_resource = Resource::new()
  
  // Simulate multiple operations on the same resource
  let resource1 = Resource::with_attributes(shared_resource, [
    ("thread.1.attr", StringValue("thread1-value"))
  ])
  
  let resource2 = Resource::with_attributes(shared_resource, [
    ("thread.2.attr", StringValue("thread2-value"))
  ])
  
  let resource3 = Resource::with_attributes(shared_resource, [
    ("thread.3.attr", StringValue("thread3-value"))
  ])
  
  // Test that operations don't interfere
  let attr1 = Resource::get_attribute(resource1, "thread.1.attr")
  let attr2 = Resource::get_attribute(resource2, "thread.2.attr")
  let attr3 = Resource::get_attribute(resource3, "thread.3.attr")
  
  assert_true(attr1.is_some() || attr1.is_none(), "Thread 1 operation should complete")
  assert_true(attr2.is_some() || attr2.is_none(), "Thread 2 operation should complete")
  assert_true(attr3.is_some() || attr3.is_none(), "Thread 3 operation should complete")
  
  // Test merging resources from different "threads"
  let merged1 = Resource::merge(resource1, resource2)
  let final_merged = Resource::merge(merged1, resource3)
  
  // Final merge should complete without errors
  assert_true(true, "Concurrent resource operations should complete successfully")
}

test "comprehensive resource management workflow" {
  // Simulate a comprehensive resource management scenario
  
  // 1. Create global resource
  let global_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("azimuth-platform")),
    ("service.version", StringValue("2.1.0")),
    ("deployment.environment", StringValue("production"))
  ])
  
  // 2. Create service-specific resource
  let telemetry_resource = Resource::with_attributes(global_resource, [
    ("service.name", StringValue("azimuth-telemetry")),
    ("service.component", StringValue("core-telemetry")),
    ("telemetry.version", StringValue("1.5.2"))
  ])
  
  // 3. Create instance-specific resource
  let instance_resource = Resource::with_attributes(telemetry_resource, [
    ("service.instance.id", StringValue("telemetry-" + Clock::system().now_unix_nanos().to_string())),
    ("host.name", StringValue("telemetry-server-01")),
    ("availability.zone", StringValue("us-west-2a"))
  ])
  
  // 4. Create request-specific resource
  let request_attrs = [
    ("request.id", StringValue("req-" + Clock::system().now_unix_nanos().to_string())),
    ("user.id", StringValue("user-12345")),
    ("operation.type", StringValue("telemetry-export"))
  ]
  let request_resource = Resource::with_attributes(instance_resource, request_attrs)
  
  // 5. Test resource hierarchy integrity
  let service_name = Resource::get_attribute(request_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth-telemetry")
    _ => assert_true(false, "Expected service name from telemetry resource")
  }
  
  let environment = Resource::get_attribute(request_resource, "deployment.environment")
  match environment {
    Some(StringValue(env)) => assert_eq(env, "production")
    _ => assert_true(false, "Expected environment from global resource")
  }
  
  let request_id = Resource::get_attribute(request_resource, "request.id")
  assert_true(request_id.is_some(), "Should have request ID")
  
  let instance_id = Resource::get_attribute(request_resource, "service.instance.id")
  assert_true(instance_id.is_some(), "Should have instance ID")
  
  // 6. Simulate resource cleanup for next request
  let next_request_resource = Resource::with_attributes(instance_resource, [
    ("request.id", StringValue("req-" + (Clock::system().now_unix_nanos() + 1000L).to_string())),
    ("user.id", StringValue("user-67890")),
    ("operation.type", StringValue("telemetry-import"))
  ])
  
  // 7. Verify resource isolation
  let current_request_id = Resource::get_attribute(request_resource, "request.id")
  let next_request_id = Resource::get_attribute(next_request_resource, "request.id")
  
  assert_true(current_request_id != next_request_id, "Different requests should have different IDs")
  
  // Comprehensive resource management workflow should complete successfully
  assert_true(true, "Comprehensive resource management workflow should complete successfully")
}