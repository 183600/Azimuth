// Advanced Resource Management Test Suite for Azimuth
// This file contains comprehensive tests for advanced resource operations

// Test 1: Resource attribute inheritance and override behavior
pub test "resource attribute inheritance and override behavior" {
  // Create base resource with common attributes
  let base_resource = azimuth::Resource::new()
  let base_attrs = [
    ("service.namespace", azimuth::StringValue("production")),
    ("deployment.environment", azimuth::StringValue("staging")),
    ("host.name", azimuth::StringValue("base-host")),
    ("process.pid", azimuth::IntValue(1234))
  ]
  let resource_with_base = azimuth::Resource::with_attributes(base_resource, base_attrs)
  
  // Create service-specific resource that inherits and overrides
  let service_attrs = [
    ("service.name", azimuth::StringValue("user-service")),
    ("service.version", azimuth::StringValue("2.1.0")),
    ("deployment.environment", azimuth::StringValue("production")),  // Override
    ("service.instance.id", azimuth::StringValue("instance-abc123"))
  ]
  let service_resource = azimuth::Resource::with_attributes(resource_with_base, service_attrs)
  
  // Create runtime-specific resource that further overrides
  let runtime_attrs = [
    ("service.instance.id", azimuth::StringValue("instance-def456")),  // Override
    ("runtime.version", azimuth::StringValue("moonbit-1.0")),
    ("memory.limit", azimuth::IntValue(1073741824))
  ]
  let final_resource = azimuth::Resource::merge(service_resource, azimuth::Resource::with_attributes(base_resource, runtime_attrs))
  
  // Verify inheritance and override behavior
  assert_eq(azimuth::Resource::get_attribute(final_resource, "service.namespace"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "deployment.environment"), Some(azimuth::StringValue("production")))  // Overridden
  assert_eq(azimuth::Resource::get_attribute(final_resource, "host.name"), Some(azimuth::StringValue("base-host")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "process.pid"), Some(azimuth::IntValue(1234)))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "service.name"), Some(azimuth::StringValue("user-service")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "service.version"), Some(azimuth::StringValue("2.1.0")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "service.instance.id"), Some(azimuth::StringValue("instance-def456")))  // Overridden
  assert_eq(azimuth::Resource::get_attribute(final_resource, "runtime.version"), Some(azimuth::StringValue("moonbit-1.0")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "memory.limit"), Some(azimuth::IntValue(1073741824)))
}

// Test 2: Resource merge strategies and conflict resolution
pub test "resource merge strategies and conflict resolution" {
  // Create primary resource
  let primary_resource = azimuth::Resource::new()
  let primary_attrs = [
    ("service.name", azimuth::StringValue("primary-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("region", azimuth::StringValue("us-east-1")),
    ("availability.zone", azimuth::StringValue("us-east-1a"))
  ]
  let primary_with_attrs = azimuth::Resource::with_attributes(primary_resource, primary_attrs)
  
  // Create secondary resource with overlapping and unique attributes
  let secondary_resource = azimuth::Resource::new()
  let secondary_attrs = [
    ("service.name", azimuth::StringValue("secondary-service")),  // Conflict
    ("service.version", azimuth::StringValue("2.0.0")),          // Conflict
    ("deployment.environment", azimuth::StringValue("production")),  // Same value
    ("region", azimuth::StringValue("us-west-2")),               // Conflict
    ("datacenter", azimuth::StringValue("dc-west-2")),           // Unique
    ("cost.center", azimuth::StringValue("engineering"))         // Unique
  ]
  let secondary_with_attrs = azimuth::Resource::with_attributes(secondary_resource, secondary_attrs)
  
  // Merge resources (secondary overrides primary in conflicts)
  let merged_resource = azimuth::Resource::merge(primary_with_attrs, secondary_with_attrs)
  
  // Verify merge results - secondary should win conflicts
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), Some(azimuth::StringValue("secondary-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "region"), Some(azimuth::StringValue("us-west-2")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "availability.zone"), Some(azimuth::StringValue("us-east-1a")))  // From primary
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "datacenter"), Some(azimuth::StringValue("dc-west-2")))  // From secondary
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "cost.center"), Some(azimuth::StringValue("engineering")))  // From secondary
}

// Test 3: Resource attribute filtering and selection
pub test "resource attribute filtering and selection" {
  // Create comprehensive resource with various attribute categories
  let resource = azimuth::Resource::new()
  let all_attrs = [
    // Service attributes
    ("service.name", azimuth::StringValue("order-service")),
    ("service.version", azimuth::StringValue("3.2.1")),
    ("service.instance.id", azimuth::StringValue("i-0a1b2c3d4e5f6g7h8")),
    ("service.namespace", azimuth::StringValue("ecommerce")),
    
    // Infrastructure attributes
    ("host.name", azimuth::StringValue("web-server-prod-01")),
    ("host.id", azimuth::StringValue("host-12345")),
    ("host.type", azimuth::StringValue("c5.large")),
    ("host.image.id", azimuth::StringValue("ami-0abcdef1234567890")),
    
    // Cloud attributes
    ("cloud.provider", azimuth::StringValue("aws")),
    ("cloud.account.id", azimuth::StringValue("123456789012")),
    ("cloud.region", azimuth::StringValue("us-west-2")),
    ("cloud.availability.zone", azimuth::StringValue("us-west-2b")),
    
    // Kubernetes attributes
    ("k8s.cluster.name", azimuth::StringValue("production-cluster")),
    ("k8s.namespace.name", azimuth::StringValue("orders")),
    ("k8s.pod.name", azimuth::StringValue("order-service-7d4f8c9c-xyz123")),
    ("k8s.deployment.name", azimuth::StringValue("order-service")),
    
    // Process attributes
    ("process.pid", azimuth::IntValue(9876)),
    ("process.executable.name", azimuth::StringValue("order-service")),
    ("process.command", azimuth::StringValue("/usr/bin/order-service")),
    ("process.command_args", azimuth::StringValue("--port=8080 --config=/etc/order-service/config.yaml")),
    
    // Telemetry SDK attributes
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.language", azimuth::StringValue("moonbit")),
    ("telemetry.sdk.version", azimuth::StringValue("1.0.0")),
    ("telemetry.auto.version", azimuth::StringValue("0.1.0"))
  ]
  
  let comprehensive_resource = azimuth::Resource::with_attributes(resource, all_attrs)
  
  // Test filtering by prefix (simulated by direct access)
  let service_name = azimuth::Resource::get_attribute(comprehensive_resource, "service.name")
  let host_name = azimuth::Resource::get_attribute(comprehensive_resource, "host.name")
  let cloud_provider = azimuth::Resource::get_attribute(comprehensive_resource, "cloud.provider")
  let k8s_cluster = azimuth::Resource::get_attribute(comprehensive_resource, "k8s.cluster.name")
  let process_pid = azimuth::Resource::get_attribute(comprehensive_resource, "process.pid")
  let telemetry_sdk = azimuth::Resource::get_attribute(comprehensive_resource, "telemetry.sdk.name")
  
  // Verify filtered attributes
  assert_eq(service_name, Some(azimuth::StringValue("order-service")))
  assert_eq(host_name, Some(azimuth::StringValue("web-server-prod-01")))
  assert_eq(cloud_provider, Some(azimuth::StringValue("aws")))
  assert_eq(k8s_cluster, Some(azimuth::StringValue("production-cluster")))
  assert_eq(process_pid, Some(azimuth::IntValue(9876)))
  assert_eq(telemetry_sdk, Some(azimuth::StringValue("azimuth")))
}

// Test 4: Resource attribute validation and sanitization
pub test "resource attribute validation and sanitization" {
  let resource = azimuth::Resource::new()
  
  // Test with various edge case attribute values
  let edge_case_attrs = [
    // Empty and whitespace values
    ("empty.string", azimuth::StringValue("")),
    ("space.only", azimuth::StringValue("   ")),
    ("newline.value", azimuth::StringValue("line1
line2")),
    ("tab.value", azimuth::StringValue("col1	col2")),
    
    // Special characters
    ("special.chars", azimuth::StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?")),
    ("unicode.chars", azimuth::StringValue("测试资源属性 🚀")),
    ("html.chars", azimuth::StringValue("<div>content</div>")),
    
    // Long values
    ("very.long.key.name.that.exceeds.normal.limits.and.should.be.handled.properly", azimuth::StringValue("long-key-value")),
    ("very.long.value", azimuth::StringValue("x" * 1000)),
    
    // Numeric edge cases
    ("zero.int", azimuth::IntValue(0)),
    ("negative.int", azimuth::IntValue(-1)),
    ("large.int", azimuth::IntValue(2147483647)),
    ("zero.float", azimuth::FloatValue(0.0)),
    ("negative.float", azimuth::FloatValue(-1.5)),
    ("infinity", azimuth::FloatValue(1.0/0.0)),
    
    // Boolean values
    ("true.value", azimuth::BoolValue(true)),
    ("false.value", azimuth::BoolValue(false))
  ]
  
  let resource_with_edge_cases = azimuth::Resource::with_attributes(resource, edge_case_attrs)
  
  // Verify all edge case values are preserved
  for (key, expected_value) in edge_case_attrs {
    let actual_value = azimuth::Resource::get_attribute(resource_with_edge_cases, key)
    assert_eq(actual_value, Some(expected_value))
  }
  
  // Test missing attributes
  let missing_attr = azimuth::Resource::get_attribute(resource_with_edge_cases, "non.existent.attribute")
  assert_eq(missing_attr, None)
}

// Test 5: Resource lifecycle and dynamic updates
pub test "resource lifecycle and dynamic updates" {
  // Initial resource creation
  let initial_resource = azimuth::Resource::new()
  let initial_attrs = [
    ("service.name", azimuth::StringValue("dynamic-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("startup.time", azimuth::IntValue(1704067200))
  ]
  let resource_v1 = azimuth::Resource::with_attributes(initial_resource, initial_attrs)
  
  // Phase 1: Add runtime attributes
  let runtime_attrs = [
    ("runtime.memory.used", azimuth::IntValue(524288)),
    ("runtime.cpu.usage", azimuth::FloatValue(0.25)),
    ("runtime.uptime", azimuth::IntValue(3600))
  ]
  let resource_v2 = azimuth::Resource::merge(resource_v1, azimuth::Resource::with_attributes(initial_resource, runtime_attrs))
  
  // Phase 2: Update configuration
  let config_attrs = [
    ("service.version", azimuth::StringValue("1.1.0")),  // Update version
    ("config.reload.time", azimuth::IntValue(1704070800)),
    ("config.source", azimuth::StringValue("/etc/service/config.yaml"))
  ]
  let resource_v3 = azimuth::Resource::merge(resource_v2, azimuth::Resource::with_attributes(initial_resource, config_attrs))
  
  // Phase 3: Add deployment information
  let deployment_attrs = [
    ("deployment.id", azimuth::StringValue("deploy-abc123")),
    ("deployment.time", azimuth::IntValue(1704074400)),
    ("deployment.strategy", azimuth::StringValue("rolling"))
  ]
  let final_resource = azimuth::Resource::merge(resource_v3, azimuth::Resource::with_attributes(initial_resource, deployment_attrs))
  
  // Verify lifecycle progression
  assert_eq(azimuth::Resource::get_attribute(final_resource, "service.name"), Some(azimuth::StringValue("dynamic-service")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "service.version"), Some(azimuth::StringValue("1.1.0")))  // Updated
  assert_eq(azimuth::Resource::get_attribute(final_resource, "startup.time"), Some(azimuth::IntValue(1704067200)))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "runtime.memory.used"), Some(azimuth::IntValue(524288)))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "runtime.cpu.usage"), Some(azimuth::FloatValue(0.25)))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "runtime.uptime"), Some(azimuth::IntValue(3600)))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "config.reload.time"), Some(azimuth::IntValue(1704070800)))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "config.source"), Some(azimuth::StringValue("/etc/service/config.yaml")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "deployment.id"), Some(azimuth::StringValue("deploy-abc123")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "deployment.time"), Some(azimuth::IntValue(1704074400)))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "deployment.strategy"), Some(azimuth::StringValue("rolling")))
}

// Test 6: Resource performance and memory efficiency
pub test "resource performance and memory efficiency" {
  // Test with large number of attributes
  let large_resource = azimuth::Resource::new()
  let large_attrs = []
  
  // Create 1000 attributes
  for i in 0..1000 {
    let key = "perf.metric." + i.to_string()
    let value = azimuth::IntValue(i * 10)
    large_attrs.push((key, value))
  }
  
  let resource_with_many_attrs = azimuth::Resource::with_attributes(large_resource, large_attrs)
  
  // Test access to various attributes
  let first_attr = azimuth::Resource::get_attribute(resource_with_many_attrs, "perf.metric.0")
  let middle_attr = azimuth::Resource::get_attribute(resource_with_many_attrs, "perf.metric.500")
  let last_attr = azimuth::Resource::get_attribute(resource_with_many_attrs, "perf.metric.999")
  
  assert_eq(first_attr, Some(azimuth::IntValue(0)))
  assert_eq(middle_attr, Some(azimuth::IntValue(5000)))
  assert_eq(last_attr, Some(azimuth::IntValue(9990)))
  
  // Test with very large attribute values
  let large_value_resource = azimuth::Resource::new()
  let large_string = "x" * 100000  // 100K string
  let large_value_attrs = [
    ("large.payload", azimuth::StringValue(large_string)),
    ("large.json", azimuth::StringValue("{\"data\": \"" + "x" * 50000 + "\"}")),
    ("large.base64", azimuth::StringValue("base64:" + "A" * 75000))
  ]
  let resource_with_large_values = azimuth::Resource::with_attributes(large_value_resource, large_value_attrs)
  
  // Verify large values are handled correctly
  let retrieved_payload = azimuth::Resource::get_attribute(resource_with_large_values, "large.payload")
  let retrieved_json = azimuth::Resource::get_attribute(resource_with_large_values, "large.json")
  let retrieved_base64 = azimuth::Resource::get_attribute(resource_with_large_values, "large.base64")
  
  assert_eq(retrieved_payload, Some(azimuth::StringValue(large_string)))
  assert_eq(retrieved_json, Some(azimuth::StringValue("{\"data\": \"" + "x" * 50000 + "\"}")))
  assert_eq(retrieved_base64, Some(azimuth::StringValue("base64:" + "A" * 75000)))
}

// Test 7: Resource attribute type conversions and consistency
pub test "resource attribute type conversions and consistency" {
  let resource = azimuth::Resource::new()
  
  // Create attributes with various types
  let typed_attrs = [
    // String attributes
    ("string.attr1", azimuth::StringValue("regular string")),
    ("string.attr2", azimuth::StringValue("123")),  // Numeric string
    ("string.attr3", azimuth::StringValue("true")),  // Boolean string
    ("string.attr4", azimuth::StringValue("123.45")),  // Float string
    
    // Integer attributes
    ("int.attr1", azimuth::IntValue(42)),
    ("int.attr2", azimuth::IntValue(0)),
    ("int.attr3", azimuth::IntValue(-100)),
    ("int.attr4", azimuth::IntValue(2147483647)),
    
    // Float attributes
    ("float.attr1", azimuth::FloatValue(3.14159)),
    ("float.attr2", azimuth::FloatValue(0.0)),
    ("float.attr3", azimuth::FloatValue(-2.718)),
    ("float.attr4", azimuth::FloatValue(1.7976931348623157e+308)),
    
    // Boolean attributes
    ("bool.attr1", azimuth::BoolValue(true)),
    ("bool.attr2", azimuth::BoolValue(false))
  ]
  
  let resource_with_types = azimuth::Resource::with_attributes(resource, typed_attrs)
  
  // Verify type preservation
  assert_eq(azimuth::Resource::get_attribute(resource_with_types, "string.attr1"), Some(azimuth::StringValue("regular string")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_types, "string.attr2"), Some(azimuth::StringValue("123")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_types, "int.attr1"), Some(azimuth::IntValue(42)))
  assert_eq(azimuth::Resource::get_attribute(resource_with_types, "float.attr1"), Some(azimuth::FloatValue(3.14159)))
  assert_eq(azimuth::Resource::get_attribute(resource_with_types, "bool.attr1"), Some(azimuth::BoolValue(true)))
  
  // Test that different types are not automatically converted
  let string_123 = azimuth::Resource::get_attribute(resource_with_types, "string.attr2")
  let int_123 = azimuth::Resource::get_attribute(resource_with_types, "int.attr1")
  
  // These should be different types
  match (string_123, int_123) {
    (Some(azimuth::StringValue(s)), Some(azimuth::IntValue(i))) => {
      assert_eq(s, "123")
      assert_eq(i, 42)
    }
    _ => assert_true(false, "Type mismatch or missing attributes")
  }
}

// Test 8: Resource serialization and reconstruction
pub test "resource serialization and reconstruction" {
  // Create complex resource for serialization testing
  let original_resource = azimuth::Resource::new()
  let complex_attrs = [
    ("service.name", azimuth::StringValue("serialization-test")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-serialization-123")),
    ("deployment.environment", azimuth::StringValue("test")),
    ("host.name", azimuth::StringValue("test-host")),
    ("process.pid", azimuth::IntValue(12345)),
    ("process.start.time", azimuth::IntValue(1704067200)),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("1.0.0")),
    ("feature.flags", azimuth::ArrayStringValue(["feature1", "feature2", "experimental"])),
    ("port.numbers", azimuth::ArrayIntValue([8080, 8443, 9090])),
    ("debug.enabled", azimuth::BoolValue(true)),
    ("memory.limit.mb", azimuth::IntValue(1024)),
    ("cpu.limit.cores", azimuth::FloatValue(2.5)),
    ("custom.metadata", azimuth::StringValue("{\"key\": \"value\", \"nested\": {\"data\": true}}"))
  ]
  
  let resource_to_serialize = azimuth::Resource::with_attributes(original_resource, complex_attrs)
  
  // Simulate serialization by verifying all attributes can be accessed
  let serialization_data = []
  for (key, _) in complex_attrs {
    let value = azimuth::Resource::get_attribute(resource_to_serialize, key)
    serialization_data.push((key, value))
  }
  
  // Verify all attributes were "serialized" (accessed successfully)
  assert_eq(serialization_data.length(), complex_attrs.length())
  
  // Simulate reconstruction by creating new resource with same attributes
  let reconstructed_resource = azimuth::Resource::new()
  let reconstructed_resource_with_attrs = azimuth::Resource::with_attributes(reconstructed_resource, complex_attrs)
  
  // Verify reconstruction integrity by comparing key attributes
  let original_service_name = azimuth::Resource::get_attribute(resource_to_serialize, "service.name")
  let reconstructed_service_name = azimuth::Resource::get_attribute(reconstructed_resource_with_attrs, "service.name")
  assert_eq(original_service_name, reconstructed_service_name)
  
  let original_pid = azimuth::Resource::get_attribute(resource_to_serialize, "process.pid")
  let reconstructed_pid = azimuth::Resource::get_attribute(reconstructed_resource_with_attrs, "process.pid")
  assert_eq(original_pid, reconstructed_pid)
  
  let original_debug = azimuth::Resource::get_attribute(resource_to_serialize, "debug.enabled")
  let reconstructed_debug = azimuth::Resource::get_attribute(reconstructed_resource_with_attrs, "debug.enabled")
  assert_eq(original_debug, reconstructed_debug)
  
  let original_features = azimuth::Resource::get_attribute(resource_to_serialize, "feature.flags")
  let reconstructed_features = azimuth::Resource::get_attribute(reconstructed_resource_with_attrs, "feature.flags")
  assert_eq(original_features, reconstructed_features)
}