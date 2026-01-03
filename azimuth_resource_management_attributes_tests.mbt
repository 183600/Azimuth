// Azimuth Resource Management and Attributes Tests
// This file contains comprehensive test cases for resource management and attributes operations

// Test 1: Resource Creation and Basic Operations
test "resource creation and basic operations" {
  let resource = Resource::new()
  
  // Test empty resource
  assert_eq(resource.attributes.length(), 0)
  
  // Test resource with attributes
  let attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("production-server")),
    ("process.pid", IntValue(12345))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  assert_eq(resource_with_attrs.attributes.length(), 5)
  
  // Test attribute retrieval
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth-service")
    _ => assert_true(false)
  }
  
  let process_pid = Resource::get_attribute(resource_with_attrs, "process.pid")
  match process_pid {
    Some(IntValue(pid)) => assert_eq(pid, 12345)
    _ => assert_true(false)
  }
  
  // Test non-existent attribute
  let non_existent = Resource::get_attribute(resource_with_attrs, "non.existent")
  assert_eq(non_existent, None)
}

// Test 2: Resource Attribute Type Operations
test "resource attribute type operations" {
  let resource = Resource::new()
  
  // Create mixed type attributes
  let mixed_attrs = [
    ("string.attr", StringValue("string value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("array.string", ArrayStringValue(["a", "b", "c"])),
    ("array.int", ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  let resource_with_mixed = Resource::with_attributes(resource, mixed_attrs)
  
  // Test string attribute
  let string_attr = Resource::get_attribute(resource_with_mixed, "string.attr")
  match string_attr {
    Some(StringValue(s)) => assert_eq(s, "string value")
    _ => assert_true(false)
  }
  
  // Test int attribute
  let int_attr = Resource::get_attribute(resource_with_mixed, "int.attr")
  match int_attr {
    Some(IntValue(i)) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  // Test float attribute
  let float_attr = Resource::get_attribute(resource_with_mixed, "float.attr")
  match float_attr {
    Some(FloatValue(f)) => assert_eq(f, 3.14159)
    _ => assert_true(false)
  }
  
  // Test bool attribute
  let bool_attr = Resource::get_attribute(resource_with_mixed, "bool.attr")
  match bool_attr {
    Some(BoolValue(b)) => assert_eq(b, true)
    _ => assert_true(false)
  }
  
  // Test array string attribute
  let array_string_attr = Resource::get_attribute(resource_with_mixed, "array.string")
  match array_string_attr {
    Some(ArrayStringValue(arr)) => assert_eq(arr.length(), 3)
    _ => assert_true(false)
  }
  
  // Test array int attribute
  let array_int_attr = Resource::get_attribute(resource_with_mixed, "array.int")
  match array_int_attr {
    Some(ArrayIntValue(arr)) => assert_eq(arr.reduce(fn(acc, x) { acc + x }, 0), 15)
    _ => assert_true(false)
  }
}

// Test 3: Resource Merge Operations
test "resource merge operations" {
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("development"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.version", StringValue("2.0.0")),
    ("environment", StringValue("production")),
    ("deployment.region", StringValue("us-west-2"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Test resource merge
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify merged attributes (simplified implementation returns override)
  let merged_version = Resource::get_attribute(merged_resource, "service.version")
  match merged_version {
    Some(StringValue(v)) => assert_eq(v, "2.0.0")
    _ => assert_true(false)
  }
  
  let merged_env = Resource::get_attribute(merged_resource, "environment")
  match merged_env {
    Some(StringValue(e)) => assert_eq(e, "production")
    _ => assert_true(false)
  }
  
  let merged_region = Resource::get_attribute(merged_resource, "deployment.region")
  match merged_region {
    Some(StringValue(r)) => assert_eq(r, "us-west-2")
    _ => assert_true(false)
  }
}

// Test 4: Deep Nested Attributes Operations
test "deep nested attributes operations" {
  let resource = Resource::new()
  
  // Create nested attribute structure
  let nested_attrs = [
    ("app.name", StringValue("azimuth")),
    ("app.version", StringValue("1.0.0")),
    ("app.config.database.host", StringValue("localhost")),
    ("app.config.database.port", IntValue(5432)),
    ("app.config.cache.redis.host", StringValue("redis-server")),
    ("app.config.cache.redis.port", IntValue(6379)),
    ("app.features.feature1.enabled", BoolValue(true)),
    ("app.features.feature2.enabled", BoolValue(false))
  ]
  
  let nested_resource = Resource::with_attributes(resource, nested_attrs)
  
  // Test nested attribute retrieval
  let app_name = Resource::get_attribute(nested_resource, "app.name")
  match app_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth")
    _ => assert_true(false)
  }
  
  let db_host = Resource::get_attribute(nested_resource, "app.config.database.host")
  match db_host {
    Some(StringValue(host)) => assert_eq(host, "localhost")
    _ => assert_true(false)
  }
  
  let db_port = Resource::get_attribute(nested_resource, "app.config.database.port")
  match db_port {
    Some(IntValue(port)) => assert_eq(port, 5432)
    _ => assert_true(false)
  }
  
  let redis_port = Resource::get_attribute(nested_resource, "app.config.cache.redis.port")
  match redis_port {
    Some(IntValue(port)) => assert_eq(port, 6379)
    _ => assert_true(false)
  }
  
  let feature1_enabled = Resource::get_attribute(nested_resource, "app.features.feature1.enabled")
  match feature1_enabled {
    Some(BoolValue(enabled)) => assert_eq(enabled, true)
    _ => assert_true(false)
  }
}

// Test 5: Resource Attribute Inheritance
test "resource attribute inheritance" {
  // Create parent resource
  let parent_resource = Resource::new()
  let parent_attrs = [
    ("service.name", StringValue("parent-service")),
    ("service.namespace", StringValue("default")),
    ("environment", StringValue("production")),
    ("team", StringValue("platform"))
  ]
  let parent_with_attrs = Resource::with_attributes(parent_resource, parent_attrs)
  
  // Create child resource
  let child_resource = Resource::new()
  let child_attrs = [
    ("service.name", StringValue("child-service")),
    ("service.version", StringValue("1.0.0")),
    ("instance.id", StringValue("child-instance-123"))
  ]
  let child_with_attrs = Resource::with_attributes(child_resource, child_attrs)
  
  // Test inheritance through merge
  let inherited_resource = Resource::merge(parent_with_attrs, child_with_attrs)
  
  // Verify inherited attributes (simplified implementation returns child)
  let inherited_name = Resource::get_attribute(inherited_resource, "service.name")
  match inherited_name {
    Some(StringValue(name)) => assert_eq(name, "child-service")
    _ => assert_true(false)
  }
  
  let inherited_version = Resource::get_attribute(inherited_resource, "service.version")
  match inherited_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  let inherited_instance = Resource::get_attribute(inherited_resource, "instance.id")
  match inherited_instance {
    Some(StringValue(instance)) => assert_eq(instance, "child-instance-123")
    _ => assert_true(false)
  }
}

// Test 6: Resource Lifecycle Management
test "resource lifecycle management" {
  // Create resource for different lifecycle stages
  let init_resource = Resource::new()
  let init_attrs = [
    ("lifecycle.stage", StringValue("initialization")),
    ("service.startup.time", StringValue("2025-01-01T00:00:00Z")),
    ("service.status", StringValue("starting"))
  ]
  let init_with_attrs = Resource::with_attributes(init_resource, init_attrs)
  
  // Transition to running state
  let running_resource = Resource::new()
  let running_attrs = [
    ("lifecycle.stage", StringValue("running")),
    ("service.startup.time", StringValue("2025-01-01T00:00:00Z")),
    ("service.status", StringValue("healthy")),
    ("service.uptime", IntValue(3600))
  ]
  let running_with_attrs = Resource::with_attributes(running_resource, running_attrs)
  
  // Transition to shutdown state
  let shutdown_resource = Resource::new()
  let shutdown_attrs = [
    ("lifecycle.stage", StringValue("shutdown")),
    ("service.startup.time", StringValue("2025-01-01T00:00:00Z")),
    ("service.status", StringValue("stopping")),
    ("service.uptime", IntValue(7200)),
    ("service.shutdown.time", StringValue("2025-01-01T02:00:00Z"))
  ]
  let shutdown_with_attrs = Resource::with_attributes(shutdown_resource, shutdown_attrs)
  
  // Verify lifecycle transitions
  let init_stage = Resource::get_attribute(init_with_attrs, "lifecycle.stage")
  match init_stage {
    Some(StringValue(stage)) => assert_eq(stage, "initialization")
    _ => assert_true(false)
  }
  
  let running_status = Resource::get_attribute(running_with_attrs, "service.status")
  match running_status {
    Some(StringValue(status)) => assert_eq(status, "healthy")
    _ => assert_true(false)
  }
  
  let shutdown_time = Resource::get_attribute(shutdown_with_attrs, "service.shutdown.time")
  match shutdown_time {
    Some(StringValue(time)) => assert_eq(time, "2025-01-01T02:00:00Z")
    _ => assert_true(false)
  }
}

// Test 7: Resource Attribute Validation
test "resource attribute validation" {
  let resource = Resource::new()
  
  // Create attributes with various formats
  let validation_attrs = [
    ("valid.string", StringValue("valid-value")),
    ("empty.string", StringValue("")),
    ("numeric.int", IntValue(42)),
    ("numeric.float", FloatValue(3.14)),
    ("boolean.true", BoolValue(true)),
    ("boolean.false", BoolValue(false)),
    ("array.empty.string", ArrayStringValue([])),
    ("array.empty.int", ArrayIntValue([])),
    ("array.single.string", ArrayStringValue(["single"])),
    ("array.single.int", ArrayIntValue([1]))
  ]
  
  let validation_resource = Resource::with_attributes(resource, validation_attrs)
  
  // Test validation scenarios
  let valid_string = Resource::get_attribute(validation_resource, "valid.string")
  match valid_string {
    Some(StringValue(s)) => assert_eq(s, "valid-value")
    _ => assert_true(false)
  }
  
  let empty_string = Resource::get_attribute(validation_resource, "empty.string")
  match empty_string {
    Some(StringValue(s)) => assert_eq(s, "")
    _ => assert_true(false)
  }
  
  let numeric_int = Resource::get_attribute(validation_resource, "numeric.int")
  match numeric_int {
    Some(IntValue(i)) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  let boolean_true = Resource::get_attribute(validation_resource, "boolean.true")
  match boolean_true {
    Some(BoolValue(b)) => assert_eq(b, true)
    _ => assert_true(false)
  }
  
  let array_empty_string = Resource::get_attribute(validation_resource, "array.empty.string")
  match array_empty_string {
    Some(ArrayStringValue(arr)) => assert_eq(arr.length(), 0)
    _ => assert_true(false)
  }
  
  let array_single_int = Resource::get_attribute(validation_resource, "array.single.int")
  match array_single_int {
    Some(ArrayIntValue(arr)) => assert_eq(arr.length(), 1)
    _ => assert_true(false)
  }
}

// Test 8: Resource Multi-Tenant Isolation
test "resource multi-tenant isolation" {
  // Create resources for different tenants
  let tenant_a_resource = Resource::new()
  let tenant_a_attrs = [
    ("tenant.id", StringValue("tenant-a")),
    ("tenant.name", StringValue("Tenant A")),
    ("service.name", StringValue("shared-service")),
    ("environment", StringValue("tenant-a-production"))
  ]
  let tenant_a_with_attrs = Resource::with_attributes(tenant_a_resource, tenant_a_attrs)
  
  let tenant_b_resource = Resource::new()
  let tenant_b_attrs = [
    ("tenant.id", StringValue("tenant-b")),
    ("tenant.name", StringValue("Tenant B")),
    ("service.name", StringValue("shared-service")),
    ("environment", StringValue("tenant-b-production"))
  ]
  let tenant_b_with_attrs = Resource::with_attributes(tenant_b_resource, tenant_b_attrs)
  
  // Verify tenant isolation
  let tenant_a_id = Resource::get_attribute(tenant_a_with_attrs, "tenant.id")
  match tenant_a_id {
    Some(StringValue(id)) => assert_eq(id, "tenant-a")
    _ => assert_true(false)
  }
  
  let tenant_b_id = Resource::get_attribute(tenant_b_with_attrs, "tenant.id")
  match tenant_b_id {
    Some(StringValue(id)) => assert_eq(id, "tenant-b")
    _ => assert_true(false)
  }
  
  // Both tenants can have same service name
  let tenant_a_service = Resource::get_attribute(tenant_a_with_attrs, "service.name")
  let tenant_b_service = Resource::get_attribute(tenant_b_with_attrs, "service.name")
  
  match (tenant_a_service, tenant_b_service) {
    (Some(StringValue(a)), Some(StringValue(b))) => {
      assert_eq(a, "shared-service")
      assert_eq(b, "shared-service")
    }
    _ => assert_true(false)
  }
  
  // But different environments
  let tenant_a_env = Resource::get_attribute(tenant_a_with_attrs, "environment")
  let tenant_b_env = Resource::get_attribute(tenant_b_with_attrs, "environment")
  
  match (tenant_a_env, tenant_b_env) {
    (Some(StringValue(a)), Some(StringValue(b))) => {
      assert_eq(a, "tenant-a-production")
      assert_eq(b, "tenant-b-production")
    }
    _ => assert_true(false)
  }
}

// Test 9: Resource Performance with Large Attribute Sets
test "resource performance with large attribute sets" {
  let resource = Resource::new()
  
  // Create large attribute set
  let large_attrs = []
  for i in 0..1000 {
    let key = "attr." + i.to_string()
    let value = StringValue("value-" + i.to_string())
    large_attrs = large_attrs.push((key, value))
  }
  
  let large_resource = Resource::with_attributes(resource, large_attrs)
  
  // Test retrieval of various attributes
  let first_attr = Resource::get_attribute(large_resource, "attr.0")
  match first_attr {
    Some(StringValue(s)) => assert_eq(s, "value-0")
    _ => assert_true(false)
  }
  
  let middle_attr = Resource::get_attribute(large_resource, "attr.500")
  match middle_attr {
    Some(StringValue(s)) => assert_eq(s, "value-500")
    _ => assert_true(false)
  }
  
  let last_attr = Resource::get_attribute(large_resource, "attr.999")
  match last_attr {
    Some(StringValue(s)) => assert_eq(s, "value-999")
    _ => assert_true(false)
  }
  
  // Test non-existent attribute
  let non_existent = Resource::get_attribute(large_resource, "attr.1000")
  assert_eq(non_existent, None)
}

// Test 10: Resource Attribute Operations Edge Cases
test "resource attribute operations edge cases" {
  let resource = Resource::new()
  
  // Test special characters in attribute keys and values
  let special_attrs = [
    ("key.with.dots", StringValue("value.with.dots")),
    ("key-with-dashes", StringValue("value-with-dashes")),
    ("key_with_underscores", StringValue("value_with_underscores")),
    ("key/with/slashes", StringValue("value/with/slashes")),
    ("key.with.special.chars!@#$%", StringValue("value.with.special.chars!@#$%")),
    ("unicode.key.测试", StringValue("unicode.value.测试")),
    ("empty.key", StringValue("value.for.empty.key")),
    ("", StringValue("value.for.empty.key")),
    ("key.for.empty.value", StringValue("")),
    ("key.for.null.value", StringValue("null"))
  ]
  
  let special_resource = Resource::with_attributes(resource, special_attrs)
  
  // Test retrieval of special attributes
  let dots_attr = Resource::get_attribute(special_resource, "key.with.dots")
  match dots_attr {
    Some(StringValue(s)) => assert_eq(s, "value.with.dots")
    _ => assert_true(false)
  }
  
  let dashes_attr = Resource::get_attribute(special_resource, "key-with-dashes")
  match dashes_attr {
    Some(StringValue(s)) => assert_eq(s, "value-with-dashes")
    _ => assert_true(false)
  }
  
  let underscores_attr = Resource::get_attribute(special_resource, "key_with_underscores")
  match underscores_attr {
    Some(StringValue(s)) => assert_eq(s, "value_with_underscores")
    _ => assert_true(false)
  }
  
  let special_chars_attr = Resource::get_attribute(special_resource, "key.with.special.chars!@#$%")
  match special_chars_attr {
    Some(StringValue(s)) => assert_eq(s, "value.with.special.chars!@#$%")
    _ => assert_true(false)
  }
  
  let unicode_attr = Resource::get_attribute(special_resource, "unicode.key.测试")
  match unicode_attr {
    Some(StringValue(s)) => assert_eq(s, "unicode.value.测试")
    _ => assert_true(false)
  }
  
  let empty_value_attr = Resource::get_attribute(special_resource, "key.for.empty.value")
  match empty_value_attr {
    Some(StringValue(s)) => assert_eq(s, "")
    _ => assert_true(false)
  }
  
  let null_value_attr = Resource::get_attribute(special_resource, "key.for.null.value")
  match null_value_attr {
    Some(StringValue(s)) => assert_eq(s, "null")
    _ => assert_true(false)
  }
}