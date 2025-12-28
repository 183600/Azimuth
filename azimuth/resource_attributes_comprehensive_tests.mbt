// Resource属性综合测试用例
test "Resource创建和基本操作" {
  let resource = Resource::new()
  assert_eq(resource.attributes.length, 0)
  
  let attrs = [("service.name", StringValue("test-service")), 
               ("service.version", StringValue("1.0.0"))]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  assert_eq(resource_with_attrs.attributes.length, 2)
}

test "Resource属性获取测试" {
  let attrs = [("service.name", StringValue("test-service")), 
               ("service.instance.id", StringValue("instance-123"))]
  let resource = Resource::with_attributes(Resource::new(), attrs)
  
  let service_name = Resource::get_attribute(resource, "service.name")
  assert_true(service_name is Some)
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_true(false)
  }
  
  let missing_attr = Resource::get_attribute(resource, "missing.key")
  assert_true(missing_attr is None)
}

test "Resource合并操作测试" {
  let base_attrs = [("service.name", StringValue("base-service"))]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [("service.name", StringValue("override-service")),
                        ("service.version", StringValue("2.0.0"))]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  let merged_name = Resource::get_attribute(merged, "service.name")
  match merged_name {
    Some(StringValue(name)) => assert_eq(name, "override-service")
    _ => assert_true(false)
  }
}

test "Resource多种属性类型测试" {
  let mixed_attrs = [
    ("string.attr", StringValue("test")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14)),
    ("bool.attr", BoolValue(true)),
    ("array.string", ArrayStringValue(["a", "b", "c"])),
    ("array.int", ArrayIntValue([1, 2, 3]))
  ]
  let resource = Resource::with_attributes(Resource::new(), mixed_attrs)
  assert_eq(resource.attributes.length, 6)
  
  // 验证不同类型的属性
  let int_attr = Resource::get_attribute(resource, "int.attr")
  match int_attr {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  let bool_attr = Resource::get_attribute(resource, "bool.attr")
  match bool_attr {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false)
  }
}