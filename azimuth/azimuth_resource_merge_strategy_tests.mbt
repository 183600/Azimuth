// Resource Merge Strategy Tests for Azimuth System
// This file contains test cases for resource attribute merging strategies

test "basic resource merging" {
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("base-instance-123"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("environment", StringValue("production"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify override takes precedence for conflicting attributes
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  let merged_service_version = Resource::get_attribute(merged_resource, "service.version")
  let merged_instance_id = Resource::get_attribute(merged_resource, "service.instance.id")
  let merged_environment = Resource::get_attribute(merged_resource, "environment")
  
  assert_eq(merged_service_name, Some(StringValue("override-service")))
  assert_eq(merged_service_version, Some(StringValue("1.0.0")))
  assert_eq(merged_instance_id, Some(StringValue("base-instance-123")))
  assert_eq(merged_environment, Some(StringValue("production")))
}

test "resource merging with array attributes" {
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.tags", ArrayStringValue(["web", "api"])),
    ("service.metrics", ArrayIntValue([100, 200]))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.tags", ArrayStringValue(["database", "cache"])),
    ("service.new.metric", ArrayIntValue([300, 400]))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify array attributes are overridden
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  let merged_tags = Resource::get_attribute(merged_resource, "service.tags")
  let merged_metrics = Resource::get_attribute(merged_resource, "service.metrics")
  let merged_new_metric = Resource::get_attribute(merged_resource, "service.new.metric")
  
  assert_eq(merged_service_name, Some(StringValue("base-service")))
  
  match merged_tags {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length, 2)
      assert_eq(values[0], "database")
      assert_eq(values[1], "cache")
    }
    _ => assert_true(false)
  }
  
  match merged_metrics {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length, 2)
      assert_eq(values[0], 100)
      assert_eq(values[1], 200)
    }
    _ => assert_true(false)
  }
  
  match merged_new_metric {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length, 2)
      assert_eq(values[0], 300)
      assert_eq(values[1], 400)
    }
    _ => assert_true(false)
  }
}

test "resource merging with different attribute types" {
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.port", IntValue(8080)),
    ("service.uptime", FloatValue(123.45)),
    ("service.active", BoolValue(true))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("service.port", IntValue(9090)),
    ("service.uptime", FloatValue(543.21)),
    ("service.active", BoolValue(false))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify all attribute types are properly overridden
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  let merged_port = Resource::get_attribute(merged_resource, "service.port")
  let merged_uptime = Resource::get_attribute(merged_resource, "service.uptime")
  let merged_active = Resource::get_attribute(merged_resource, "service.active")
  
  assert_eq(merged_service_name, Some(StringValue("override-service")))
  assert_eq(merged_port, Some(IntValue(9090)))
  assert_eq(merged_uptime, Some(FloatValue(543.21)))
  assert_eq(merged_active, Some(BoolValue(false)))
}

test "resource merging with empty resources" {
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let empty_resource = Resource::new()
  
  // Merge empty resource into base
  let merged_empty_into_base = Resource::merge(base_with_attrs, empty_resource)
  let merged_service_name = Resource::get_attribute(merged_empty_into_base, "service.name")
  let merged_version = Resource::get_attribute(merged_empty_into_base, "service.version")
  
  assert_eq(merged_service_name, Some(StringValue("base-service")))
  assert_eq(merged_version, Some(StringValue("1.0.0")))
  
  // Merge base into empty resource
  let merged_base_into_empty = Resource::merge(empty_resource, base_with_attrs)
  let merged_service_name2 = Resource::get_attribute(merged_base_into_empty, "service.name")
  let merged_version2 = Resource::get_attribute(merged_base_into_empty, "service.version")
  
  assert_eq(merged_service_name2, Some(StringValue("base-service")))
  assert_eq(merged_version2, Some(StringValue("1.0.0")))
}

test "resource merging with special characters" {
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("测试服务")),
    ("service.description", StringValue("Service with special chars: @#$%^&*()")),
    ("service.json", StringValue("{\"key\": \"value\", \"array\": [1, 2, 3]}"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("覆盖服务")),
    ("service.emoji", StringValue("🚀 Service with emoji"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify special characters are preserved
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  let merged_description = Resource::get_attribute(merged_resource, "service.description")
  let merged_json = Resource::get_attribute(merged_resource, "service.json")
  let merged_emoji = Resource::get_attribute(merged_resource, "service.emoji")
  
  assert_eq(merged_service_name, Some(StringValue("覆盖服务")))
  assert_eq(merged_description, Some(StringValue("Service with special chars: @#$%^&*()")))
  assert_eq(merged_json, Some(StringValue("{\"key\": \"value\", \"array\": [1, 2, 3]}")))
  assert_eq(merged_emoji, Some(StringValue("🚀 Service with emoji")))
}

test "resource merging with large attribute sets" {
  let base_resource = Resource::new()
  let mut base_attrs = []
  
  // Create large attribute set
  for i = 0; i < 100; i = i + 1 {
    base_attrs = Array::push(base_attrs, ("base.attr." + i.to_string(), StringValue("base.value." + i.to_string())))
  }
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let mut override_attrs = []
  
  // Create override attribute set
  for i = 0; i < 50; i = i + 1 {
    override_attrs = Array::push(override_attrs, ("base.attr." + i.to_string(), StringValue("override.value." + i.to_string())))
  }
  
  // Add new attributes
  for i = 0; i < 50; i = i + 1 {
    override_attrs = Array::push(override_attrs, ("new.attr." + i.to_string(), StringValue("new.value." + i.to_string())))
  }
  
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify first 50 attributes are overridden
  let merged_attr_0 = Resource::get_attribute(merged_resource, "base.attr.0")
  let merged_attr_49 = Resource::get_attribute(merged_resource, "base.attr.49")
  let merged_attr_50 = Resource::get_attribute(merged_resource, "base.attr.50")
  let merged_attr_99 = Resource::get_attribute(merged_resource, "base.attr.99")
  let merged_new_attr_0 = Resource::get_attribute(merged_resource, "new.attr.0")
  let merged_new_attr_49 = Resource::get_attribute(merged_resource, "new.attr.49")
  
  assert_eq(merged_attr_0, Some(StringValue("override.value.0")))
  assert_eq(merged_attr_49, Some(StringValue("override.value.49")))
  assert_eq(merged_attr_50, Some(StringValue("base.value.50")))
  assert_eq(merged_attr_99, Some(StringValue("base.value.99")))
  assert_eq(merged_new_attr_0, Some(StringValue("new.value.0")))
  assert_eq(merged_new_attr_49, Some(StringValue("new.value.49")))
}

test "resource merging with null and empty values" {
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.empty", StringValue("")),
    ("service.zero", IntValue(0)),
    ("service.false", BoolValue(false))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("")),
    ("service.empty", StringValue("not-empty")),
    ("service.zero", IntValue(1)),
    ("service.false", BoolValue(true))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify empty and zero values are properly handled
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  let merged_empty = Resource::get_attribute(merged_resource, "service.empty")
  let merged_zero = Resource::get_attribute(merged_resource, "service.zero")
  let merged_false = Resource::get_attribute(merged_resource, "service.false")
  
  assert_eq(merged_service_name, Some(StringValue("")))
  assert_eq(merged_empty, Some(StringValue("not-empty")))
  assert_eq(merged_zero, Some(IntValue(1)))
  assert_eq(merged_false, Some(BoolValue(true)))
}

test "resource merging with nested attribute keys" {
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.config.database.host", StringValue("base-db-host")),
    ("service.config.database.port", IntValue(5432)),
    ("service.config.cache.redis.host", StringValue("base-redis-host"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.config.database.host", StringValue("override-db-host")),
    ("service.config.cache.redis.host", StringValue("override-redis-host")),
    ("service.config.new.setting", StringValue("new-value"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify nested attribute keys are properly merged
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  let merged_db_host = Resource::get_attribute(merged_resource, "service.config.database.host")
  let merged_db_port = Resource::get_attribute(merged_resource, "service.config.database.port")
  let merged_redis_host = Resource::get_attribute(merged_resource, "service.config.cache.redis.host")
  let merged_new_setting = Resource::get_attribute(merged_resource, "service.config.new.setting")
  
  assert_eq(merged_service_name, Some(StringValue("base-service")))
  assert_eq(merged_db_host, Some(StringValue("override-db-host")))
  assert_eq(merged_db_port, Some(IntValue(5432)))
  assert_eq(merged_redis_host, Some(StringValue("override-redis-host")))
  assert_eq(merged_new_setting, Some(StringValue("new-value")))
}