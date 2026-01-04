// Azimuth 数据一致性验证测试用例
// 测试遥测系统中数据在各种操作下的一致性保证

test "属性数据一致性测试" {
  // 创建初始属性集合
  let initial_attrs = azimuth::Attributes {
    values: [
      ("user.id", azimuth::AttributeValue::StringValue("user123")),
      ("session.id", azimuth::AttributeValue::StringValue("session456")),
      ("operation.name", azimuth::AttributeValue::StringValue("test_operation")),
      ("operation.duration", azimuth::AttributeValue::IntValue(150)),
      ("operation.success", azimuth::AttributeValue::BoolValue(true))
    ]
  }
  
  // 验证初始数据
  assert_eq(initial_attrs.values.length(), 5)
  
  // 添加新属性
  let updated_attrs = azimuth::Attributes {
    values: initial_attrs.values + [
      ("new.attribute", azimuth::AttributeValue::StringValue("new_value"))
    ]
  }
  
  // 验证添加后的一致性
  assert_eq(updated_attrs.values.length(), 6)
  
  // 验证原有属性保持不变
  let mut found_original_attrs = 0
  for (key, value) in updated_attrs.values {
    match key {
      "user.id" => {
        match value {
          azimuth::AttributeValue::StringValue(v) => {
            assert_eq(v, "user123")
            found_original_attrs = found_original_attrs + 1
          }
          _ => assert_true(false)
        }
      }
      "session.id" => {
        match value {
          azimuth::AttributeValue::StringValue(v) => {
            assert_eq(v, "session456")
            found_original_attrs = found_original_attrs + 1
          }
          _ => assert_true(false)
        }
      }
      "operation.name" => {
        match value {
          azimuth::AttributeValue::StringValue(v) => {
            assert_eq(v, "test_operation")
            found_original_attrs = found_original_attrs + 1
          }
          _ => assert_true(false)
        }
      }
      "operation.duration" => {
        match value {
          azimuth::AttributeValue::IntValue(v) => {
            assert_eq(v, 150)
            found_original_attrs = found_original_attrs + 1
          }
          _ => assert_true(false)
        }
      }
      "operation.success" => {
        match value {
          azimuth::AttributeValue::BoolValue(v) => {
            assert_true(v)
            found_original_attrs = found_original_attrs + 1
          }
          _ => assert_true(false)
        }
      }
      "new.attribute" => {
        match value {
          azimuth::AttributeValue::StringValue(v) => {
            assert_eq(v, "new_value")
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false) // 不应该有其他键
    }
  }
  
  assert_eq(found_original_attrs, 5) // 所有原始属性都应该被找到
}

test "跨度上下文一致性测试" {
  // 创建跨度上下文
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_id = "00f067aa0ba902b7"
  let original_span_context = azimuth::SpanContext {
    trace_id: trace_id,
    span_id: span_id,
    sampled: true,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 验证原始上下文
  assert_eq(original_span_context.trace_id, trace_id)
  assert_eq(original_span_context.span_id, span_id)
  assert_true(original_span_context.sampled)
  assert_eq(original_span_context.trace_state, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  
  // 创建子跨度上下文（继承父跨度的追踪ID）
  let child_span_id = "b7ad6b7169203331"
  let child_span_context = azimuth::SpanContext {
    trace_id: original_span_context.trace_id, // 继承追踪ID
    span_id: child_span_id, // 新的跨度ID
    sampled: original_span_context.sampled, // 继承采样标志
    trace_state: original_span_context.trace_state // 继承追踪状态
  }
  
  // 验证父子关系的一致性
  assert_eq(child_span_context.trace_id, original_span_context.trace_id)
  assert_eq(child_span_context.span_id, child_span_id)
  assert_eq(child_span_context.sampled, original_span_context.sampled)
  assert_eq(child_span_context.trace_state, original_span_context.trace_state)
  
  // 验证父子跨度的关系
  assert_not_eq(child_span_context.span_id, original_span_context.span_id)
  assert_eq(child_span_context.trace_id, original_span_context.trace_id)
  
  // 创建孙跨度上下文
  let grandchild_span_id = "9c5a7e3d8f1b4c2a"
  let grandchild_span_context = azimuth::SpanContext {
    trace_id: original_span_context.trace_id, // 仍然继承原始追踪ID
    span_id: grandchild_span_id,
    sampled: original_span_context.sampled,
    trace_state: original_span_context.trace_state
  }
  
  // 验证三代跨度的一致性
  assert_eq(grandchild_span_context.trace_id, original_span_context.trace_id)
  assert_eq(grandchild_span_context.trace_id, child_span_context.trace_id)
  assert_not_eq(grandchild_span_context.span_id, original_span_context.span_id)
  assert_not_eq(grandchild_span_context.span_id, child_span_context.span_id)
  assert_eq(grandchild_span_context.sampled, original_span_context.sampled)
  assert_eq(grandchild_span_context.trace_state, original_span_context.trace_state)
}

test "行李传播一致性测试" {
  // 创建初始行李
  let initial_baggage = azimuth::Baggage {
    entries: [
      ("user.id", "user123"),
      ("session.id", "session456"),
      ("request.id", "req789")
    ]
  }
  
  // 验证初始行李
  assert_eq(initial_baggage.entries.length(), 3)
  
  // 模拟跨服务传播（添加新的条目）
  let propagated_baggage = azimuth::Baggage {
    entries: initial_baggage.entries + [
      ("service.name", "authentication"),
      ("operation.id", "op456")
    ]
  }
  
  // 验证传播后的行李
  assert_eq(propagated_baggage.entries.length(), 5)
  
  // 验证原始条目保持不变
  let mut found_original_entries = 0
  for (key, value) in propagated_baggage.entries {
    match key {
      "user.id" => {
        assert_eq(value, "user123")
        found_original_entries = found_original_entries + 1
      }
      "session.id" => {
        assert_eq(value, "session456")
        found_original_entries = found_original_entries + 1
      }
      "request.id" => {
        assert_eq(value, "req789")
        found_original_entries = found_original_entries + 1
      }
      "service.name" => {
        assert_eq(value, "authentication")
      }
      "operation.id" => {
        assert_eq(value, "op456")
      }
      _ => assert_true(false) // 不应该有其他键
    }
  }
  
  assert_eq(found_original_entries, 3) // 所有原始条目都应该被找到
  
  // 模拟进一步传播（更新现有条目）
  let updated_baggage = azimuth::Baggage {
    entries: [
      ("user.id", "user123"), // 保持不变
      ("session.id", "session789"), // 更新
      ("request.id", "req789"), // 保持不变
      ("service.name", "authentication"), // 保持不变
      ("operation.id", "op789"), // 更新
      ("new.service", "authorization") // 新增
    ]
  }
  
  // 验证更新后的行李
  assert_eq(updated_baggage.entries.length(), 6)
  
  // 验证特定条目的更新
  let mut found_session = false
  let mut found_operation = false
  let mut found_new_service = false
  
  for (key, value) in updated_baggage.entries {
    match key {
      "session.id" => {
        assert_eq(value, "session789") // 应该是更新后的值
        found_session = true
      }
      "operation.id" => {
        assert_eq(value, "op789") // 应该是更新后的值
        found_operation = true
      }
      "new.service" => {
        assert_eq(value, "authorization") // 新增的条目
        found_new_service = true
      }
      _ => () // 其他键
    }
  }
  
  assert_true(found_session)
  assert_true(found_operation)
  assert_true(found_new_service)
}

test "资源合并一致性测试" {
  // 创建基础资源
  let base_resource = azimuth::Resource {
    attributes: [
      ("service.name", azimuth::AttributeValue::StringValue("base-service")),
      ("service.version", azimuth::AttributeValue::StringValue("1.0.0")),
      ("host.name", azimuth::AttributeValue::StringValue("host1"))
    ]
  }
  
  // 创建覆盖资源
  let override_resource = azimuth::Resource {
    attributes: [
      ("service.version", azimuth::AttributeValue::StringValue("2.0.0")), // 覆盖
      ("host.name", azimuth::AttributeValue::StringValue("host2")), // 覆盖
      ("environment", azimuth::AttributeValue::StringValue("production")) // 新增
    ]
  }
  
  // 模拟资源合并
  let mut merged_attributes = []
  
  // 首先添加基础资源的所有属性
  for (key, value) in base_resource.attributes {
    merged_attributes = merged_attributes + [(key, value)]
  }
  
  // 然后添加/更新覆盖资源的属性
  for (key, value) in override_resource.attributes {
    let mut found = false
    let mut updated_attributes = []
    
    // 检查是否已存在该键
    for (existing_key, existing_value) in merged_attributes {
      if existing_key == key {
        // 覆盖现有值
        updated_attributes = updated_attributes + [(key, value)]
        found = true
      } else {
        // 保留现有值
        updated_attributes = updated_attributes + [(existing_key, existing_value)]
      }
    }
    
    if !found {
      // 添加新属性
      updated_attributes = updated_attributes + [(key, value)]
    }
    
    merged_attributes = updated_attributes
  }
  
  let merged_resource = azimuth::Resource { attributes: merged_attributes }
  
  // 验证合并结果
  assert_eq(merged_resource.attributes.length(), 4) // 3个基础 + 1个新增
  
  // 验证特定属性的合并结果
  let mut service_name_found = false
  let mut service_version_found = false
  let mut host_name_found = false
  let mut environment_found = false
  
  for (key, value) in merged_resource.attributes {
    match key {
      "service.name" => {
        match value {
          azimuth::AttributeValue::StringValue(v) => {
            assert_eq(v, "base-service") // 应该保持原值
            service_name_found = true
          }
          _ => assert_true(false)
        }
      }
      "service.version" => {
        match value {
          azimuth::AttributeValue::StringValue(v) => {
            assert_eq(v, "2.0.0") // 应该被覆盖
            service_version_found = true
          }
          _ => assert_true(false)
        }
      }
      "host.name" => {
        match value {
          azimuth::AttributeValue::StringValue(v) => {
            assert_eq(v, "host2") // 应该被覆盖
            host_name_found = true
          }
          _ => assert_true(false)
        }
      }
      "environment" => {
        match value {
          azimuth::AttributeValue::StringValue(v) => {
            assert_eq(v, "production") // 新增的属性
            environment_found = true
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false) // 不应该有其他键
    }
  }
  
  assert_true(service_name_found)
  assert_true(service_version_found)
  assert_true(host_name_found)
  assert_true(environment_found)
}

test "上下文传播一致性测试" {
  // 创建根上下文
  let root_context = azimuth::Context { data: None }
  
  // 添加第一个键值对
  let key1 = azimuth::ContextKey::new("trace.id")
  let context1 = azimuth::Context {
    data: Some(("trace.id", "trace-12345"))
  }
  
  // 添加第二个键值对（基于前一个上下文）
  let key2 = azimuth::ContextKey::new("user.id")
  let context2 = azimuth::Context {
    data: Some(("user.id", "user-67890"))
  }
  
  // 添加第三个键值对
  let key3 = azimuth::ContextKey::new("operation.id")
  let context3 = azimuth::Context {
    data: Some(("operation.id", "op-11111"))
  }
  
  // 验证上下文链的一致性
  // 注意：这是一个简化的上下文实现，实际中可能有更复杂的链式结构
  
  match context1.data {
    Some((key, value)) => {
      assert_eq(key, "trace.id")
      assert_eq(value, "trace-12345")
    }
    None => assert_true(false)
  }
  
  match context2.data {
    Some((key, value)) => {
      assert_eq(key, "user.id")
      assert_eq(value, "user-67890")
    }
    None => assert_true(false)
  }
  
  match context3.data {
    Some((key, value)) => {
      assert_eq(key, "operation.id")
      assert_eq(value, "op-11111")
    }
    None => assert_true(false)
  }
  
  // 模拟上下文传播过程中的数据提取
  let context_list = [context1, context2, context3]
  let mut extracted_values = []
  
  for context in context_list {
    match context.data {
      Some((key, value)) => {
        extracted_values = extracted_values + [(key, value)]
      }
      None => assert_true(false)
    }
  }
  
  // 验证提取的数据
  assert_eq(extracted_values.length(), 3)
  
  // 验证特定值
  let mut trace_id_found = false
  let mut user_id_found = false
  let mut operation_id_found = false
  
  for (key, value) in extracted_values {
    match key {
      "trace.id" => {
        assert_eq(value, "trace-12345")
        trace_id_found = true
      }
      "user.id" => {
        assert_eq(value, "user-67890")
        user_id_found = true
      }
      "operation.id" => {
        assert_eq(value, "op-11111")
        operation_id_found = true
      }
      _ => assert_true(false) // 不应该有其他键
    }
  }
  
  assert_true(trace_id_found)
  assert_true(user_id_found)
  assert_true(operation_id_found)
}

test "序列化反序列化一致性测试" {
  // 创建原始数据结构
  let original_attrs = azimuth::Attributes {
    values: [
      ("string.key", azimuth::AttributeValue::StringValue("string_value")),
      ("int.key", azimuth::AttributeValue::IntValue(42)),
      ("float.key", azimuth::AttributeValue::FloatValue(3.14)),
      ("bool.key", azimuth::AttributeValue::BoolValue(true))
    ]
  }
  
  // 模拟序列化过程
  let mut serialized_string = "{\"attributes\":["
  for i in 0..original_attrs.values.length() {
    if i > 0 {
      serialized_string = serialized_string + ","
    }
    
    let (key, value) = original_attrs.values[i]
    serialized_string = serialized_string + "{\"key\":\"" + key + "\"," +
    
    match value {
      azimuth::AttributeValue::StringValue(s) => {
        serialized_string = serialized_string + "\"type\":\"string\",\"value\":\"" + s + "\""
      }
      azimuth::AttributeValue::IntValue(i) => {
        serialized_string = serialized_string + "\"type\":\"int\",\"value\":" + i.to_string()
      }
      azimuth::AttributeValue::FloatValue(f) => {
        serialized_string = serialized_string + "\"type\":\"float\",\"value\":" + f.to_string()
      }
      azimuth::AttributeValue::BoolValue(b) => {
        let bool_str = if b { "true" } else { "false" }
        serialized_string = serialized_string + "\"type\":\"bool\",\"value\":" + bool_str
      }
      _ => {
        serialized_string = serialized_string + "\"type\":\"unknown\",\"value\":null"
      }
    }
    
    serialized_string = serialized_string + "}"
  }
  serialized_string = serialized_string + "]}"
  
  // 验证序列化结果包含所有必要信息
  assert_true(serialized_string.contains("\"key\":\"string.key\""))
  assert_true(serialized_string.contains("\"value\":\"string_value\""))
  assert_true(serialized_string.contains("\"key\":\"int.key\""))
  assert_true(serialized_string.contains("\"value\":42"))
  assert_true(serialized_string.contains("\"key\":\"float.key\""))
  assert_true(serialized_string.contains("\"value\":3.14"))
  assert_true(serialized_string.contains("\"key\":\"bool.key\""))
  assert_true(serialized_string.contains("\"value\":true"))
  
  // 模拟反序列化过程（简化版）
  let mut deserialized_attrs = azimuth::Attributes { values: [] }
  
  // 添加反序列化的属性
  deserialized_attrs.values = [
    ("string.key", azimuth::AttributeValue::StringValue("string_value")),
    ("int.key", azimuth::AttributeValue::IntValue(42)),
    ("float.key", azimuth::AttributeValue::FloatValue(3.14)),
    ("bool.key", azimuth::AttributeValue::BoolValue(true))
  ]
  
  // 验证反序列化结果与原始数据的一致性
  assert_eq(deserialized_attrs.values.length(), original_attrs.values.length())
  
  // 逐个验证属性
  for i in 0..deserialized_attrs.values.length() {
    let (deser_key, deser_value) = deserialized_attrs.values[i]
    let (orig_key, orig_value) = original_attrs.values[i]
    
    assert_eq(deser_key, orig_key)
    
    // 验证值的一致性
    match (deser_value, orig_value) {
      (azimuth::AttributeValue::StringValue(d), azimuth::AttributeValue::StringValue(o)) => {
        assert_eq(d, o)
      }
      (azimuth::AttributeValue::IntValue(d), azimuth::AttributeValue::IntValue(o)) => {
        assert_eq(d, o)
      }
      (azimuth::AttributeValue::FloatValue(d), azimuth::AttributeValue::FloatValue(o)) => {
        assert_eq(d, o)
      }
      (azimuth::AttributeValue::BoolValue(d), azimuth::AttributeValue::BoolValue(o)) => {
        assert_eq(d, o)
      }
      _ => assert_true(false) // 类型不匹配
    }
  }
}

test "跨服务数据一致性测试" {
  // 模拟服务A生成的数据
  let service_a_attrs = azimuth::Attributes {
    values: [
      ("service.name", azimuth::AttributeValue::StringValue("service-a")),
      ("trace.id", azimuth::AttributeValue::StringValue("trace-12345")),
      ("span.id", azimuth::AttributeValue::StringValue("span-a-11111")),
      ("user.id", azimuth::AttributeValue::StringValue("user-67890")),
      ("operation.name", azimuth::AttributeValue::StringValue("authenticate"))
    ]
  }
  
  // 模拟服务B接收并增强数据
  let service_b_attrs = azimuth::Attributes {
    values: service_a_attrs.values + [
      ("service.name", azimuth::AttributeValue::StringValue("service-b")), // 更新服务名
      ("span.id", azimuth::AttributeValue::StringValue("span-b-22222")), // 新的跨度ID
      ("parent.span", azimuth::AttributeValue::StringValue("span-a-11111")), // 父跨度引用
      ("operation.name", azimuth::AttributeValue::StringValue("authorize")), // 更新操作名
      ("auth.result", azimuth::AttributeValue::BoolValue(true)), // 新增结果
      ("auth.duration", azimuth::AttributeValue::IntValue(25)) // 新增耗时
    ]
  }
  
  // 模拟服务C进一步处理数据
  let service_c_attrs = azimuth::Attributes {
    values: [
      // 保留关键追踪信息
      ("trace.id", azimuth::AttributeValue::StringValue("trace-12345")),
      ("user.id", azimuth::AttributeValue::StringValue("user-67890")),
      // 添加服务C的信息
      ("service.name", azimuth::AttributeValue::StringValue("service-c")),
      ("span.id", azimuth::AttributeValue::StringValue("span-c-33333")),
      ("parent.span", azimuth::AttributeValue::StringValue("span-b-22222")),
      ("operation.name", azimuth::AttributeValue::StringValue("validate_token")),
      ("validation.result", azimuth::AttributeValue::BoolValue(true)),
      ("validation.details", azimuth::AttributeValue::StringValue("token_valid"))
    ]
  }
  
  // 验证跨服务数据的一致性
  
  // 1. 验证追踪ID在所有服务中保持一致
  let mut trace_id_consistent = false
  let mut user_id_consistent = false
  
  for (key, value) in service_a_attrs.values {
    match key {
      "trace.id" => {
        match value {
          azimuth::AttributeValue::StringValue(trace_id) => {
            // 检查服务B和服务C中的追踪ID是否一致
            let mut service_b_trace_id = ""
            let mut service_c_trace_id = ""
            
            for (b_key, b_value) in service_b_attrs.values {
              if b_key == "trace.id" {
                match b_value {
                  azimuth::AttributeValue::StringValue(b_trace) => {
                    service_b_trace_id = b_trace
                  }
                  _ => assert_true(false)
                }
              }
            }
            
            for (c_key, c_value) in service_c_attrs.values {
              if c_key == "trace.id" {
                match c_value {
                  azimuth::AttributeValue::StringValue(c_trace) => {
                    service_c_trace_id = c_trace
                  }
                  _ => assert_true(false)
                }
              }
            }
            
            assert_eq(trace_id, service_b_trace_id)
            assert_eq(trace_id, service_c_trace_id)
            trace_id_consistent = true
          }
          _ => assert_true(false)
        }
      }
      "user.id" => {
        match value {
          azimuth::AttributeValue::StringValue(user_id) => {
            // 检查服务B和服务C中的用户ID是否一致
            let mut service_b_user_id = ""
            let mut service_c_user_id = ""
            
            for (b_key, b_value) in service_b_attrs.values {
              if b_key == "user.id" {
                match b_value {
                  azimuth::AttributeValue::StringValue(b_user) => {
                    service_b_user_id = b_user
                  }
                  _ => assert_true(false)
                }
              }
            }
            
            for (c_key, c_value) in service_c_attrs.values {
              if c_key == "user.id" {
                match c_value {
                  azimuth::AttributeValue::StringValue(c_user) => {
                    service_c_user_id = c_user
                  }
                  _ => assert_true(false)
                }
              }
            }
            
            assert_eq(user_id, service_b_user_id)
            assert_eq(user_id, service_c_user_id)
            user_id_consistent = true
          }
          _ => assert_true(false)
        }
      }
      _ => () // 其他键
    }
  }
  
  assert_true(trace_id_consistent)
  assert_true(user_id_consistent)
  
  // 2. 验证父子关系的正确性
  let mut parent_relation_correct = false
  
  // 查找服务B中的parent.span是否指向服务A的span.id
  let mut service_a_span_id = ""
  let mut service_b_parent_span = ""
  let mut service_b_span_id = ""
  let mut service_c_parent_span = ""
  
  for (key, value) in service_a_attrs.values {
    if key == "span.id" {
      match value {
        azimuth::AttributeValue::StringValue(span_id) => {
          service_a_span_id = span_id
        }
        _ => assert_true(false)
      }
    }
  }
  
  for (key, value) in service_b_attrs.values {
    match key {
      "parent.span" => {
        match value {
          azimuth::AttributeValue::StringValue(parent_span) => {
            service_b_parent_span = parent_span
          }
          _ => assert_true(false)
        }
      }
      "span.id" => {
        match value {
          azimuth::AttributeValue::StringValue(span_id) => {
            service_b_span_id = span_id
          }
          _ => assert_true(false)
        }
      }
      _ => () // 其他键
    }
  }
  
  for (key, value) in service_c_attrs.values {
    if key == "parent.span" {
      match value {
        azimuth::AttributeValue::StringValue(parent_span) => {
          service_c_parent_span = parent_span
        }
        _ => assert_true(false)
      }
    }
  }
  
  // 验证父子关系
  assert_eq(service_b_parent_span, service_a_span_id) // B的父跨度应该是A的跨度
  assert_eq(service_c_parent_span, service_b_span_id) // C的父跨度应该是B的跨度
  parent_relation_correct = true
  
  assert_true(parent_relation_correct)
}