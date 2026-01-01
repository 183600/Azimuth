// Azimuth Resource Merge Strategy Test Suite
// This file contains test cases for resource merge strategy functionality

// Test 1: Basic resource merge operations
test "basic resource merge operations" {
  // Test empty resource merge
  let empty_resource1 = Resource::new()
  let empty_resource2 = Resource::new()
  let merged_empty = Resource::merge(empty_resource1, empty_resource2)
  
  assert_eq(empty_resource1.attributes.length(), 0)
  assert_eq(empty_resource2.attributes.length(), 0)
  
  // Test merge with single attribute
  let resource1 = Resource::with_attributes(empty_resource1, [("service.name", StringValue("service1"))])
  let resource2 = Resource::with_attributes(empty_resource2, [("service.version", StringValue("1.0.0"))])
  let merged1 = Resource::merge(resource1, resource2)
  
  assert_eq(Resource::get_attribute(merged1, "service.name"), Some(StringValue("service1")))  // Simplified implementation
  assert_eq(Resource::get_attribute(merged1, "service.version"), Some(StringValue("1.0.0")))
  
  // Test merge with multiple attributes
  let resource3 = Resource::with_attributes(empty_resource1, [
    ("service.name", StringValue("service1")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("test"))
  ])
  
  let resource4 = Resource::with_attributes(empty_resource2, [
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("test-host"))
  ])
  
  let merged2 = Resource::merge(resource3, resource4)
  
  assert_eq(Resource::get_attribute(merged2, "service.name"), Some(StringValue("service1")))  // Simplified
  assert_eq(Resource::get_attribute(merged2, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(merged2, "deployment.environment"), Some(StringValue("test")))
  assert_eq(Resource::get_attribute(merged2, "service.instance.id"), Some(StringValue("instance-123")))
  assert_eq(Resource::get_attribute(merged2, "host.name"), Some(StringValue("test-host")))
}

// Test 2: Resource merge with attribute conflicts
test "resource merge with attribute conflicts" {
  let base_resource = Resource::new()
  
  // Create resources with conflicting attributes
  let resource1 = Resource::with_attributes(base_resource, [
    ("service.name", StringValue("original-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("development"))
  ])
  
  let resource2 = Resource::with_attributes(base_resource, [
    ("service.name", StringValue("override-service")),  // Conflict
    ("service.version", StringValue("2.0.0")),          // Conflict
    ("host.name", StringValue("override-host"))         // New attribute
  ])
  
  let merged = Resource::merge(resource1, resource2)
  
  // In simplified implementation, override resource wins
  assert_eq(Resource::get_attribute(merged, "service.name"), Some(StringValue("override-service")))
  assert_eq(Resource::get_attribute(merged, "service.version"), Some(StringValue("2.0.0")))
  assert_eq(Resource::get_attribute(merged, "deployment.environment"), Some(StringValue("development")))  // From resource1
  assert_eq(Resource::get_attribute(merged, "host.name"), Some(StringValue("override-host")))
  
  // Test multiple levels of merging
  let resource3 = Resource::with_attributes(base_resource, [
    ("service.name", StringValue("final-service")),     // Override again
    ("service.instance.id", StringValue("instance-456"))
  ])
  
  let merged2 = Resource::merge(merged, resource3)
  
  assert_eq(Resource::get_attribute(merged2, "service.name"), Some(StringValue("final-service")))
  assert_eq(Resource::get_attribute(merged2, "service.version"), Some(StringValue("2.0.0")))
  assert_eq(Resource::get_attribute(merged2, "deployment.environment"), Some(StringValue("development")))
  assert_eq(Resource::get_attribute(merged2, "host.name"), Some(StringValue("override-host")))
  assert_eq(Resource::get_attribute(merged2, "service.instance.id"), Some(StringValue("instance-456")))
}

// Test 3: Resource merge with different attribute types
test "resource merge with different attribute types" {
  let base_resource = Resource::new()
  
  // Create resources with different attribute types
  let resource1 = Resource::with_attributes(base_resource, [
    ("string.attr", StringValue("string.value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14)),
    ("bool.attr", BoolValue(true))
  ])
  
  let resource2 = Resource::with_attributes(base_resource, [
    ("string.attr", StringValue("override.string")),  // Override string
    ("int.attr", IntValue(100)),                      // Override int
    ("array.string.attr", ArrayStringValue(["a", "b", "c"])),
    ("array.int.attr", ArrayIntValue([1, 2, 3]))
  ])
  
  let merged = Resource::merge(resource1, resource2)
  
  // Verify merged attributes
  assert_eq(Resource::get_attribute(merged, "string.attr"), Some(StringValue("override.string")))
  assert_eq(Resource::get_attribute(merged, "int.attr"), Some(IntValue(100)))
  assert_eq(Resource::get_attribute(merged, "float.attr"), Some(FloatValue(3.14)))  // From resource1
  assert_eq(Resource::get_attribute(merged, "bool.attr"), Some(BoolValue(true)))     // From resource1
  assert_eq(Resource::get_attribute(merged, "array.string.attr"), Some(ArrayStringValue(["a", "b", "c"])))
  assert_eq(Resource::get_attribute(merged, "array.int.attr"), Some(ArrayIntValue([1, 2, 3])))
  
  // Test overriding with different types
  let resource3 = Resource::with_attributes(base_resource, [
    ("string.attr", IntValue(999)),        // Change type from string to int
    ("float.attr", StringValue("3.14159")), // Change type from float to string
    ("bool.attr", IntValue(1))             // Change type from bool to int
  ])
  
  let merged2 = Resource::merge(merged, resource3)
  
  assert_eq(Resource::get_attribute(merged2, "string.attr"), Some(IntValue(999)))
  assert_eq(Resource::get_attribute(merged2, "float.attr"), Some(StringValue("3.14159")))
  assert_eq(Resource::get_attribute(merged2, "bool.attr"), Some(IntValue(1)))
}

// Test 4: Resource merge with special characters and Unicode
test "resource merge with special characters and unicode" {
  let base_resource = Resource::new()
  
  // Create resources with special characters and Unicode
  let resource1 = Resource::with_attributes(base_resource, [
    ("key.with-dashes", StringValue("value-with-dashes")),
    ("key_with_underscores", StringValue("value_with_underscores")),
    ("key.with.dots", StringValue("value.with.dots")),
    ("unicode.key.测试", StringValue("unicode.value.测试 🚀"))
  ])
  
  let resource2 = Resource::with_attributes(base_resource, [
    ("key.with-dashes", StringValue("override-value-with-dashes")),  // Override
    ("special.chars!@#", StringValue("special.value!@#")),
    ("emoji.key.🔥", StringValue("emoji.value.🚀"))
  ])
  
  let merged = Resource::merge(resource1, resource2)
  
  // Verify merged attributes
  assert_eq(Resource::get_attribute(merged, "key.with-dashes"), Some(StringValue("override-value-with-dashes")))
  assert_eq(Resource::get_attribute(merged, "key_with_underscores"), Some(StringValue("value_with_underscores")))
  assert_eq(Resource::get_attribute(merged, "key.with.dots"), Some(StringValue("value.with.dots")))
  assert_eq(Resource::get_attribute(merged, "unicode.key.测试"), Some(StringValue("unicode.value.测试 🚀")))
  assert_eq(Resource::get_attribute(merged, "special.chars!@#"), Some(StringValue("special.value!@#")))
  assert_eq(Resource::get_attribute(merged, "emoji.key.🔥"), Some(StringValue("emoji.value.🚀")))
}

// Test 5: Resource merge with edge cases
test "resource merge with edge cases" {
  let base_resource = Resource::new()
  
  // Test with empty strings
  let resource1 = Resource::with_attributes(base_resource, [
    ("empty.string", StringValue("")),
    ("normal.string", StringValue("normal"))
  ])
  
  let resource2 = Resource::with_attributes(base_resource, [
    ("empty.string", StringValue("not.empty")),  // Override empty
    ("another.empty", StringValue(""))
  ])
  
  let merged1 = Resource::merge(resource1, resource2)
  
  assert_eq(Resource::get_attribute(merged1, "empty.string"), Some(StringValue("not.empty")))
  assert_eq(Resource::get_attribute(merged1, "normal.string"), Some(StringValue("normal")))
  assert_eq(Resource::get_attribute(merged1, "another.empty"), Some(StringValue("")))
  
  // Test with extreme numeric values
  let resource3 = Resource::with_attributes(base_resource, [
    ("max.int", IntValue(2147483647)),
    ("min.int", IntValue(-2147483648)),
    ("zero.int", IntValue(0)),
    ("max.float", FloatValue(1.7976931348623157e+308)),
    ("min.float", FloatValue(-1.7976931348623157e+308)),
    ("zero.float", FloatValue(0.0))
  ])
  
  let resource4 = Resource::with_attributes(base_resource, [
    ("max.int", IntValue(100)),           // Override max int
    ("new.max.int", IntValue(2147483647)), // New max int
    ("zero.float", FloatValue(-0.0))       // Override zero float
  ])
  
  let merged2 = Resource::merge(resource3, resource4)
  
  assert_eq(Resource::get_attribute(merged2, "max.int"), Some(IntValue(100)))
  assert_eq(Resource::get_attribute(merged2, "min.int"), Some(IntValue(-2147483648)))
  assert_eq(Resource::get_attribute(merged2, "zero.int"), Some(IntValue(0)))
  assert_eq(Resource::get_attribute(merged2, "max.float"), Some(FloatValue(1.7976931348623157e+308)))
  assert_eq(Resource::get_attribute(merged2, "min.float"), Some(FloatValue(-1.7976931348623157e+308)))
  assert_eq(Resource::get_attribute(merged2, "zero.float"), Some(FloatValue(-0.0)))
  assert_eq(Resource::get_attribute(merged2, "new.max.int"), Some(IntValue(2147483647)))
  
  // Test with very long keys and values
  let long_key = "this.is.a.very.long.key.name.that.exceeds.normal.limits.and.tests.boundary.conditions"
  let long_value = "this.is.a.very.long.value.that.contains.many.characters.and.tests.boundary.conditions.to.ensure.proper.handling"
  
  let resource5 = Resource::with_attributes(base_resource, [
    (long_key, StringValue(long_value))
  ])
  
  let resource6 = Resource::with_attributes(base_resource, [
    (long_key, StringValue("override.long.value"))  // Override long value
  ])
  
  let merged3 = Resource::merge(resource5, resource6)
  
  assert_eq(Resource::get_attribute(merged3, long_key), Some(StringValue("override.long.value")))
}

// Test 6: Resource merge with complex scenarios
test "resource merge with complex scenarios" {
  let base_resource = Resource::new()
  
  // Create a realistic service resource
  let service_resource = Resource::with_attributes(base_resource, [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("payment-service-12345")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("payment-prod-1")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("5.15.0-102-generic")),
    ("process.id", IntValue(12345)),
    ("process.name", StringValue("payment-service")),
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.version", StringValue("0.1.0")),
    ("telemetry.sdk.language", StringValue("moonbit"))
  ])
  
  // Create a runtime resource
  let runtime_resource = Resource::with_attributes(base_resource, [
    ("service.name", StringValue("payment-service")),  // Same value
    ("service.version", StringValue("1.2.4")),          // Different version
    ("runtime.name", StringValue("moonbit")),
    ("runtime.version", StringValue("0.1.0")),
    ("cpu.count", IntValue(4)),
    ("memory.total", IntValue(8589934592)),
    ("memory.available", IntValue(4294967296)),
    ("disk.total", IntValue(107374182400)),
    ("disk.available", IntValue(53687091200)),
    ("network.interface", StringValue("eth0")),
    ("network.ip", StringValue("10.0.1.100"))
  ])
  
  // Create a business context resource
  let business_resource = Resource::with_attributes(base_resource, [
    ("business.unit", StringValue("payments")),
    ("business.region", StringValue("us-west")),
    ("business.tier", StringValue("critical")),
    ("cost.center", StringValue("engineering")),
    ("compliance.level", StringValue("pci-dss")),
    ("data.classification", StringValue("sensitive"))
  ])
  
  // Merge all resources
  let merged1 = Resource::merge(service_resource, runtime_resource)
  let final_merged = Resource::merge(merged1, business_resource)
  
  // Verify final merged resource
  assert_eq(Resource::get_attribute(final_merged, "service.name"), Some(StringValue("payment-service")))
  assert_eq(Resource::get_attribute(final_merged, "service.version"), Some(StringValue("1.2.4")))  // From runtime
  assert_eq(Resource::get_attribute(final_merged, "service.instance.id"), Some(StringValue("payment-service-12345")))
  assert_eq(Resource::get_attribute(final_merged, "deployment.environment"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(final_merged, "host.name"), StringValue("payment-prod-1"))
  assert_eq(Resource::get_attribute(final_merged, "os.type"), Some(StringValue("linux")))
  assert_eq(Resource::get_attribute(final_merged, "os.version"), Some(StringValue("5.15.0-102-generic")))
  assert_eq(Resource::get_attribute(final_merged, "process.id"), Some(IntValue(12345)))
  assert_eq(Resource::get_attribute(final_merged, "process.name"), Some(StringValue("payment-service")))
  assert_eq(Resource::get_attribute(final_merged, "telemetry.sdk.name"), Some(StringValue("azimuth")))
  assert_eq(Resource::get_attribute(final_merged, "telemetry.sdk.version"), Some(StringValue("0.1.0")))
  assert_eq(Resource::get_attribute(final_merged, "telemetry.sdk.language"), Some(StringValue("moonbit")))
  assert_eq(Resource::get_attribute(final_merged, "runtime.name"), Some(StringValue("moonbit")))
  assert_eq(Resource::get_attribute(final_merged, "runtime.version"), Some(StringValue("0.1.0")))
  assert_eq(Resource::get_attribute(final_merged, "cpu.count"), Some(IntValue(4)))
  assert_eq(Resource::get_attribute(final_merged, "memory.total"), Some(IntValue(8589934592)))
  assert_eq(Resource::get_attribute(final_merged, "memory.available"), Some(IntValue(4294967296)))
  assert_eq(Resource::get_attribute(final_merged, "disk.total"), Some(IntValue(107374182400)))
  assert_eq(Resource::get_attribute(final_merged, "disk.available"), Some(IntValue(53687091200)))
  assert_eq(Resource::get_attribute(final_merged, "network.interface"), Some(StringValue("eth0")))
  assert_eq(Resource::get_attribute(final_merged, "network.ip"), Some(StringValue("10.0.1.100")))
  assert_eq(Resource::get_attribute(final_merged, "business.unit"), Some(StringValue("payments")))
  assert_eq(Resource::get_attribute(final_merged, "business.region"), Some(StringValue("us-west")))
  assert_eq(Resource::get_attribute(final_merged, "business.tier"), Some(StringValue("critical")))
  assert_eq(Resource::get_attribute(final_merged, "cost.center"), Some(StringValue("engineering")))
  assert_eq(Resource::get_attribute(final_merged, "compliance.level"), Some(StringValue("pci-dss")))
  assert_eq(Resource::get_attribute(final_merged, "data.classification"), Some(StringValue("sensitive")))
}

// Test 7: Resource merge with array attributes
test "resource merge with array attributes" {
  let base_resource = Resource::new()
  
  // Create resources with array attributes
  let resource1 = Resource::with_attributes(base_resource, [
    ("string.array", ArrayStringValue(["item1", "item2", "item3"])),
    ("int.array", ArrayIntValue([1, 2, 3, 4, 5])),
    ("mixed.attrs", StringValue("not.array"))
  ])
  
  let resource2 = Resource::with_attributes(base_resource, [
    ("string.array", ArrayStringValue(["override1", "override2"])),  // Override array
    ("new.string.array", ArrayStringValue(["new1", "new2"])),
    ("int.array", IntValue(999))  // Change array to single value
  ])
  
  let merged = Resource::merge(resource1, resource2)
  
  // Verify merged array attributes
  assert_eq(Resource::get_attribute(merged, "string.array"), Some(ArrayStringValue(["override1", "override2"])))
  assert_eq(Resource::get_attribute(merged, "new.string.array"), Some(ArrayStringValue(["new1", "new2"])))
  assert_eq(Resource::get_attribute(merged, "int.array"), Some(IntValue(999)))  // Changed to int
  assert_eq(Resource::get_attribute(merged, "mixed.attrs"), Some(StringValue("not.array")))
  
  // Test with empty arrays
  let resource3 = Resource::with_attributes(base_resource, [
    ("empty.string.array", ArrayStringValue([])),
    ("empty.int.array", ArrayIntValue([]))
  ])
  
  let resource4 = Resource::with_attributes(base_resource, [
    ("empty.string.array", ArrayStringValue(["not.empty"])),  // Override empty
    ("empty.int.array", ArrayIntValue([1, 2, 3]))
  ])
  
  let merged2 = Resource::merge(resource3, resource4)
  
  assert_eq(Resource::get_attribute(merged2, "empty.string.array"), Some(ArrayStringValue(["not.empty"])))
  assert_eq(Resource::get_attribute(merged2, "empty.int.array"), Some(ArrayIntValue([1, 2, 3])))
}

// Test 8: Resource merge performance and scalability
test "resource merge performance and scalability" {
  let base_resource = Resource::new()
  
  // Create resource with many attributes
  let large_resource1 = Resource::with_attributes(base_resource, [
    ("attr.1", StringValue("value.1")),
    ("attr.2", StringValue("value.2")),
    ("attr.3", StringValue("value.3")),
    ("attr.4", StringValue("value.4")),
    ("attr.5", StringValue("value.5"))
  ])
  
  let large_resource2 = Resource::with_attributes(base_resource, [
    ("attr.6", StringValue("value.6")),
    ("attr.7", StringValue("value.7")),
    ("attr.8", StringValue("value.8")),
    ("attr.9", StringValue("value.9")),
    ("attr.10", StringValue("value.10"))
  ])
  
  // Merge large resources
  let merged_large = Resource::merge(large_resource1, large_resource2)
  
  // Verify merged attributes
  assert_eq(Resource::get_attribute(merged_large, "attr.1"), Some(StringValue("value.1")))
  assert_eq(Resource::get_attribute(merged_large, "attr.5"), Some(StringValue("value.5")))
  assert_eq(Resource::get_attribute(merged_large, "attr.6"), Some(StringValue("value.6")))
  assert_eq(Resource::get_attribute(merged_large, "attr.10"), Some(StringValue("value.10")))
  
  // Test sequential merging
  let mut current_resource = base_resource
  
  for i in 0..10 {
    let resource_to_merge = Resource::with_attributes(base_resource, [
      ("sequential.attr." + i.to_string(), StringValue("sequential.value." + i.to_string()))
    ])
    current_resource = Resource::merge(current_resource, resource_to_merge)
  }
  
  // Verify sequential merge results
  assert_eq(Resource::get_attribute(current_resource, "sequential.attr.0"), Some(StringValue("sequential.value.0")))
  assert_eq(Resource::get_attribute(current_resource, "sequential.attr.5"), Some(StringValue("sequential.value.5")))
  assert_eq(Resource::get_attribute(current_resource, "sequential.attr.9"), Some(StringValue("sequential.value.9")))
  
  // Test merge with conflicting attributes in sequence
  let conflict_base = Resource::with_attributes(base_resource, [
    ("conflict.attr", StringValue("initial"))
  ])
  
  let mut conflict_resource = conflict_base
  
  for i in 0..5 {
    let resource_to_merge = Resource::with_attributes(base_resource, [
      ("conflict.attr", StringValue("iteration." + i.to_string()))
    ])
    conflict_resource = Resource::merge(conflict_resource, resource_to_merge)
  }
  
  // Verify final conflict result (last one wins)
  assert_eq(Resource::get_attribute(conflict_resource, "conflict.attr"), Some(StringValue("iteration.4")))
}