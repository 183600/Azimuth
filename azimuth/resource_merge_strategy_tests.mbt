// Resource Merge Strategy Tests
// Testing resource merging strategies and attribute resolution

test "resource merge with empty base resource" {
  // Test merging when base resource is empty
  let base_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // Verify merged resource contains override attributes
  let service_name = Resource::get_attribute(merged, "service.name")
  let service_version = Resource::get_attribute(merged, "service.version")
  let environment = Resource::get_attribute(merged, "deployment.environment")
  
  assert_eq(service_name, Some(StringValue("test-service")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(environment, Some(StringValue("production")))
}

test "resource merge with empty override resource" {
  // Test merging when override resource is empty
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let override_resource = Resource::new()
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // Verify merged resource contains base attributes
  let service_name = Resource::get_attribute(merged, "service.name")
  let instance_id = Resource::get_attribute(merged, "service.instance.id")
  
  assert_eq(service_name, Some(StringValue("base-service")))
  assert_eq(instance_id, Some(StringValue("instance-123")))
}

test "resource merge with conflicting attributes" {
  // Test merging when both resources have the same attribute keys
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("region", StringValue("us-west-1"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("service.version", StringValue("2.0.0")),
    ("zone", StringValue("us-west-1a"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // Verify override takes precedence for conflicting keys
  let service_name = Resource::get_attribute(merged, "service.name")
  let service_version = Resource::get_attribute(merged, "service.version")
  
  // Verify non-conflicting attributes are preserved
  let region = Resource::get_attribute(merged, "region")
  let zone = Resource::get_attribute(merged, "zone")
  
  assert_eq(service_name, Some(StringValue("override-service")))
  assert_eq(service_version, Some(StringValue("2.0.0")))
  assert_eq(region, Some(StringValue("us-west-1")))
  assert_eq(zone, Some(StringValue("us-west-1a")))
}

test "resource merge with different attribute types" {
  // Test merging resources with different attribute value types
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("port", IntValue(8080)),
    ("enabled", BoolValue(true)),
    ("tags", ArrayStringValue(["base", "test"]))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("port", IntValue(9090)),
    ("ratio", FloatValue(0.95)),
    ("counters", ArrayIntValue([1, 2, 3]))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // Verify different types are handled correctly
  let service_name = Resource::get_attribute(merged, "service.name")
  let port = Resource::get_attribute(merged, "port")
  let enabled = Resource::get_attribute(merged, "enabled")
  let ratio = Resource::get_attribute(merged, "ratio")
  let tags = Resource::get_attribute(merged, "tags")
  let counters = Resource::get_attribute(merged, "counters")
  
  assert_eq(service_name, Some(StringValue("override-service")))
  assert_eq(port, Some(IntValue(9090)))
  assert_eq(enabled, Some(BoolValue(true)))
  assert_eq(ratio, Some(FloatValue(0.95)))
  
  match tags {
    Some(ArrayStringValue(arr)) => {
      assert_eq(arr.length(), 2)
      assert_eq(arr[0], "base")
      assert_eq(arr[1], "test")
    }
    _ => assert_false(true)
  }
  
  match counters {
    Some(ArrayIntValue(arr)) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1)
      assert_eq(arr[2], 3)
    }
    _ => assert_false(true)
  }
}

test "resource merge chain operation" {
  // Test chaining multiple resource merges
  let resource1 = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("service-1")),
    ("version", StringValue("1.0.0"))
  ])
  
  let resource2 = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("service-2")),
    ("environment", StringValue("staging"))
  ])
  
  let resource3 = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("service-3")),
    ("region", StringValue("eu-central"))
  ])
  
  // Chain merges: resource1 -> resource2 -> resource3
  let merged1 = Resource::merge(resource1, resource2)
  let final_merged = Resource::merge(merged1, resource3)
  
  // Verify final merged state
  let service_name = Resource::get_attribute(final_merged, "service.name")
  let version = Resource::get_attribute(final_merged, "version")
  let environment = Resource::get_attribute(final_merged, "environment")
  let region = Resource::get_attribute(final_merged, "region")
  
  assert_eq(service_name, Some(StringValue("service-3")))
  assert_eq(version, Some(StringValue("1.0.0")))
  assert_eq(environment, Some(StringValue("staging")))
  assert_eq(region, Some(StringValue("eu-central")))
}

test "resource merge with special characters and unicode" {
  // Test merging resources with special characters and unicode
  let base_attrs = [
    ("service.name", StringValue("测试服务")),
    ("description", StringValue("Service with special chars: !@#$%^&*()")),
    ("unicode.key", StringValue("🚀🔥💧"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [
    ("service.name", StringValue("更新服务")),
    ("emoji.support", StringValue("✅❌⚠️"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // Verify unicode and special characters are preserved
  let service_name = Resource::get_attribute(merged, "service.name")
  let description = Resource::get_attribute(merged, "description")
  let unicode_key = Resource::get_attribute(merged, "unicode.key")
  let emoji_support = Resource::get_attribute(merged, "emoji.support")
  
  assert_eq(service_name, Some(StringValue("更新服务")))
  assert_eq(description, Some(StringValue("Service with special chars: !@#$%^&*()")))
  assert_eq(unicode_key, Some(StringValue("🚀🔥💧")))
  assert_eq(emoji_support, Some(StringValue("✅❌⚠️")))
}

test "resource merge with large attribute sets" {
  // Test merging resources with many attributes
  let mut base_attrs = [] : Array[(String, AttributeValue)]
  for i = 0; i < 50; i++ {
    base_attrs = base_attrs.push(("base.key." + i.to_string(), StringValue("base.value." + i.to_string())))
  }
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let mut override_attrs = [] : Array[(String, AttributeValue)]
  for i = 0; i < 50; i++ {
    override_attrs = override_attrs.push(("override.key." + i.to_string(), StringValue("override.value." + i.to_string())))
  }
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // Verify all attributes are present
  let base_key_0 = Resource::get_attribute(merged, "base.key.0")
  let base_key_49 = Resource::get_attribute(merged, "base.key.49")
  let override_key_0 = Resource::get_attribute(merged, "override.key.0")
  let override_key_49 = Resource::get_attribute(merged, "override.key.49")
  
  assert_eq(base_key_0, Some(StringValue("base.value.0")))
  assert_eq(base_key_49, Some(StringValue("base.value.49")))
  assert_eq(override_key_0, Some(StringValue("override.value.0")))
  assert_eq(override_key_49, Some(StringValue("override.value.49")))
}