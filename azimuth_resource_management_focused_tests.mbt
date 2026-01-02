// Azimuth 资源管理专项测试
// 专注于测试遥测系统的资源管理和合并策略

// 测试1: Resource 基本操作
test "resource_basic_operations" {
  let empty_resource = Resource { attributes: [] }
  let simple_resource = Resource {
    attributes: [
      ("service.name", AttributeValue::StringValue("test-service"))
    ]
  }
  let complex_resource = Resource {
    attributes: [
      ("service.name", AttributeValue::StringValue("azimuth-service")),
      ("service.version", AttributeValue::StringValue("1.0.0")),
      ("service.instance.id", AttributeValue::StringValue("instance-123")),
      ("host.name", AttributeValue::StringValue("host-01")),
      ("process.pid", AttributeValue::IntValue(12345))
    ]
  }
  
  // 验证空资源
  assert_eq(empty_resource.attributes.length(), 0)
  
  // 验证简单资源
  assert_eq(simple_resource.attributes.length(), 1)
  match simple_resource.attributes[0] {
    (k, v) => {
      assert_eq(k, "service.name")
      match v {
        AttributeValue::StringValue(name) => assert_eq(name, "test-service")
        _ => assert_true(false)
      }
    }
  }
  
  // 验证复杂资源
  assert_eq(complex_resource.attributes.length(), 5)
  
  // 查找特定属性
  let service_name = complex_resource.attributes.find(fn(attr) { attr.0 == "service.name" })
  match service_name {
    Some((k, v)) => {
      assert_eq(k, "service.name")
      match v {
        AttributeValue::StringValue(name) => assert_eq(name, "azimuth-service")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let process_pid = complex_resource.attributes.find(fn(attr) { attr.0 == "process.pid" })
  match process_pid {
    Some((k, v)) => {
      assert_eq(k, "process.pid")
      match v {
        AttributeValue::IntValue(pid) => assert_eq(pid, 12345)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// 测试2: Resource 属性合并策略
test "resource_attribute_merge_strategy" {
  let primary_resource = Resource {
    attributes: [
      ("service.name", AttributeValue::StringValue("primary-service")),
      ("service.version", AttributeValue::StringValue("1.0.0")),
      ("deployment.environment", AttributeValue::StringValue("production"))
    ]
  }
  
  let secondary_resource = Resource {
    attributes: [
      ("service.name", AttributeValue::StringValue("secondary-service")), // 冲突键
      ("host.name", AttributeValue::StringValue("host-01")), // 新键
      ("service.instance.id", AttributeValue::StringValue("instance-456")) // 新键
    ]
  }
  
  // 模拟资源合并策略：主资源优先，但添加不冲突的次要资源属性
  let merged_attributes = [] // 在实际实现中，这里会有合并逻辑
  let primary_attrs = primary_resource.attributes
  let secondary_attrs = secondary_resource.attributes
  
  // 添加主资源的所有属性
  let mut merged = primary_attrs.to_array()
  
  // 添加次要资源中不冲突的属性
  secondary_attrs.each(fn(sec_attr) {
    let key_exists = primary_attrs.any(fn(attr) { attr.0 == sec_attr.0 })
    if not key_exists {
      merged.push(sec_attr)
    }
  })
  
  // 验证合并结果
  assert_eq(merged.length(), 5) // 3个主资源属性 + 2个不冲突的次要资源属性
  
  // 验证主资源的service.name被保留
  let service_name = merged.find(fn(attr) { attr.0 == "service.name" })
  match service_name {
    Some((k, v)) => {
      assert_eq(k, "service.name")
      match v {
        AttributeValue::StringValue(name) => assert_eq(name, "primary-service") // 主资源的值
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证次要资源的新属性被添加
  let host_name = merged.find(fn(attr) { attr.0 == "host.name" })
  match host_name {
    Some((k, v)) => {
      assert_eq(k, "host.name")
      match v {
        AttributeValue::StringValue(name) => assert_eq(name, "host-01")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let instance_id = merged.find(fn(attr) { attr.0 == "service.instance.id" })
  match instance_id {
    Some((k, v)) => {
      assert_eq(k, "service.instance.id")
      match v {
        AttributeValue::StringValue(id) => assert_eq(id, "instance-456")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// 测试3: 多级资源合并
test "multi_level_resource_merge" {
  let base_resource = Resource {
    attributes: [
      ("service.name", AttributeValue::StringValue("base-service")),
      ("service.version", AttributeValue::StringValue("1.0.0"))
    ]
  }
  
  let layer1_resource = Resource {
    attributes: [
      ("deployment.environment", AttributeValue::StringValue("staging")),
      ("host.name", AttributeValue::StringValue("host-01"))
    ]
  }
  
  let layer2_resource = Resource {
    attributes: [
      ("service.instance.id", AttributeValue::StringValue("instance-123")),
      ("process.pid", AttributeValue::IntValue(54321))
    ]
  }
  
  // 模拟多级资源合并
  let mut merged = base_resource.attributes.to_array()
  
  // 添加第一层
  layer1_resource.attributes.each(fn(attr) {
    let key_exists = merged.any_fn(m => m.0 == attr.0)
    if not key_exists {
      merged.push(attr)
    }
  })
  
  // 添加第二层
  layer2_resource.attributes.each(fn(attr) {
    let key_exists = merged.any_fn(m => m.0 == attr.0)
    if not key_exists {
      merged.push(attr)
    }
  })
  
  // 验证合并结果
  assert_eq(merged.length(), 6)
  
  // 验证所有预期的属性都存在
  let expected_keys = ["service.name", "service.version", "deployment.environment", "host.name", "service.instance.id", "process.pid"]
  expected_keys.each_fn(key => {
    let found = merged.any_fn(attr => attr.0 == key)
    assert_true(found)
  })
}

// 测试4: 资源属性类型转换
test "resource_attribute_type_conversion" {
  let string_resource = Resource {
    attributes: [
      ("string.value", AttributeValue::StringValue("test")),
      ("numeric.value", AttributeValue::StringValue("123")),
      ("boolean.value", AttributeValue::StringValue("true"))
    ]
  }
  
  let typed_resource = Resource {
    attributes: [
      ("string.value", AttributeValue::StringValue("test")),
      ("numeric.value", AttributeValue::IntValue(123)),
      ("boolean.value", AttributeValue::BoolValue(true))
    ]
  }
  
  // 验证字符串资源
  let string_val = string_resource.attributes.find(fn(attr) { attr.0 == "string.value" })
  match string_val {
    Some((k, v)) => {
      assert_eq(k, "string.value")
      match v {
        AttributeValue::StringValue(s) => assert_eq(s, "test")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let numeric_string_val = string_resource.attributes.find(fn(attr) { attr.0 == "numeric.value" })
  match numeric_string_val {
    Some((k, v)) => {
      assert_eq(k, "numeric.value")
      match v {
        AttributeValue::StringValue(s) => assert_eq(s, "123")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证类型化资源
  let typed_numeric_val = typed_resource.attributes.find(fn(attr) { attr.0 == "numeric.value" })
  match typed_numeric_val {
    Some((k, v)) => {
      assert_eq(k, "numeric.value")
      match v {
        AttributeValue::IntValue(i) => assert_eq(i, 123)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let typed_boolean_val = typed_resource.attributes.find(fn(attr) { attr.0 == "boolean.value" })
  match typed_boolean_val {
    Some((k, v)) => {
      assert_eq(k, "boolean.value")
      match v {
        AttributeValue::BoolValue(b) => assert_true(b)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// 测试5: 资源属性过滤
test "resource_attribute_filtering" {
  let full_resource = Resource {
    attributes: [
      ("service.name", AttributeValue::StringValue("test-service")),
      ("service.version", AttributeValue::StringValue("1.0.0")),
      ("service.instance.id", AttributeValue::StringValue("instance-123")),
      ("host.name", AttributeValue::StringValue("host-01")),
      ("process.pid", AttributeValue::IntValue(12345)),
      ("custom.attribute", AttributeValue::StringValue("custom-value")),
      ("internal.metric", AttributeValue::IntValue(999))
    ]
  }
  
  // 过滤出服务相关属性
  let service_attributes = full_resource.attributes.filter(fn(attr) { 
    attr.0.starts_with("service.")
  })
  
  // 验证过滤结果
  assert_eq(service_attributes.length(), 3)
  service_attributes.each(fn(attr) {
    assert_true(attr.0.starts_with("service."))
  })
  
  // 过滤出字符串类型属性
  let string_attributes = full_resource.attributes.filter(fn(attr) {
    match attr.1 {
      AttributeValue::StringValue(_) => true
      _ => false
    }
  })
  
  // 验证过滤结果
  assert_eq(string_attributes.length(), 5)
  string_attributes.each(fn(attr) {
    match attr.1 {
      AttributeValue::StringValue(_) => assert_true(true)
      _ => assert_true(false)
    }
  })
  
  // 过滤出数值类型属性
  let int_attributes = full_resource.attributes.filter(fn(attr) {
    match attr.1 {
      AttributeValue::IntValue(_) => true
      _ => false
    }
  })
  
  // 验证过滤结果
  assert_eq(int_attributes.length(), 2)
  int_attributes.each(fn(attr) {
    match attr.1 {
      AttributeValue::IntValue(_) => assert_true(true)
      _ => assert_true(false)
    }
  })
}

// 测试6: 资源属性验证
test "resource_attribute_validation" {
  let valid_resource = Resource {
    attributes: [
      ("service.name", AttributeValue::StringValue("valid-service")),
      ("service.version", AttributeValue::StringValue("1.0.0")),
      ("process.pid", AttributeValue::IntValue(12345))
    ]
  }
  
  let invalid_resource = Resource {
    attributes: [
      ("service.name", AttributeValue::StringValue("")), // 空字符串
      ("service.version", AttributeValue::StringValue("1.0.0")),
      ("process.pid", AttributeValue::IntValue(-1)) // 负数PID
    ]
  }
  
  // 验证有效资源
  let valid_service_name = valid_resource.attributes.find(fn(attr) { attr.0 == "service.name" })
  match valid_service_name {
    Some((k, v)) => {
      assert_eq(k, "service.name")
      match v {
        AttributeValue::StringValue(name) => {
          assert_true(name.length() > 0)
          assert_eq(name, "valid-service")
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let valid_pid = valid_resource.attributes.find(fn(attr) { attr.0 == "process.pid" })
  match valid_pid {
    Some((k, v)) => {
      assert_eq(k, "process.pid")
      match v {
        AttributeValue::IntValue(pid) => {
          assert_true(pid >= 0)
          assert_eq(pid, 12345)
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证无效资源
  let invalid_service_name = invalid_resource.attributes.find(fn(attr) { attr.0 == "service.name" })
  match invalid_service_name {
    Some((k, v)) => {
      assert_eq(k, "service.name")
      match v {
        AttributeValue::StringValue(name) => {
          assert_eq(name.length(), 0) // 空字符串
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let invalid_pid = invalid_resource.attributes.find(fn(attr) { attr.0 == "process.pid" })
  match invalid_pid {
    Some((k, v)) => {
      assert_eq(k, "process.pid")
      match v {
        AttributeValue::IntValue(pid) => {
          assert_true(pid < 0) // 负数PID
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// 测试7: 资源属性序列化准备
test "resource_attribute_serialization_preparation" {
  let mixed_type_resource = Resource {
    attributes: [
      ("string.attr", AttributeValue::StringValue("string-value")),
      ("int.attr", AttributeValue::IntValue(42)),
      ("float.attr", AttributeValue::FloatValue(3.14)),
      ("bool.attr", AttributeValue::BoolValue(true)),
      ("array.string.attr", AttributeValue::ArrayStringValue(["a", "b", "c"])),
      ("array.int.attr", AttributeValue::ArrayIntValue([1, 2, 3]))
    ]
  }
  
  // 验证所有属性类型都可以正确处理
  mixed_type_resource.attributes.each(fn(attr) {
    match attr.1 {
      AttributeValue::StringValue(v) => {
        assert_eq(v, "string-value")
      }
      AttributeValue::IntValue(v) => {
        assert_eq(v, 42)
      }
      AttributeValue::FloatValue(v) => {
        assert_true(v > 3.0 && v < 3.2)
      }
      AttributeValue::BoolValue(v) => {
        assert_true(v)
      }
      AttributeValue::ArrayStringValue(arr) => {
        assert_eq(arr.length(), 3)
        assert_eq(arr[0], "a")
        assert_eq(arr[1], "b")
        assert_eq(arr[2], "c")
      }
      AttributeValue::ArrayIntValue(arr) => {
        assert_eq(arr.length(), 3)
        assert_eq(arr[0], 1)
        assert_eq(arr[1], 2)
        assert_eq(arr[2], 3)
      }
    }
  })
}

// 测试8: 资源生命周期管理
test "resource_lifecycle_management" {
  // 创建资源
  let initial_resource = Resource {
    attributes: [
      ("service.name", AttributeValue::StringValue("lifecycle-test")),
      ("service.version", AttributeValue::StringValue("1.0.0"))
    ]
  }
  
  // 验证初始状态
  assert_eq(initial_resource.attributes.length(), 2)
  
  // 模拟资源更新
  let updated_resource = Resource {
    attributes: [
      ("service.name", AttributeValue::StringValue("lifecycle-test")),
      ("service.version", AttributeValue::StringValue("2.0.0")), // 版本更新
      ("deployment.environment", AttributeValue::StringValue("production")) // 新属性
    ]
  }
  
  // 验证更新后的状态
  assert_eq(updated_resource.attributes.length(), 3)
  
  let new_version = updated_resource.attributes.find(fn(attr) { attr.0 == "service.version" })
  match new_version {
    Some((k, v)) => {
      assert_eq(k, "service.version")
      match v {
        AttributeValue::StringValue(version) => assert_eq(version, "2.0.0")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let new_env = updated_resource.attributes.find(fn(attr) { attr.0 == "deployment.environment" })
  match new_env {
    Some((k, v)) => {
      assert_eq(k, "deployment.environment")
      match v {
        AttributeValue::StringValue(env) => assert_eq(env, "production")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}