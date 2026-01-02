// Azimuth Concurrent Safety Test Suite
// 测试并发场景下的安全性，确保在多线程/多协程环境下的数据一致性和操作安全性

// 注意：MoonBit的并发模型可能与传统多线程不同，这里主要测试数据结构的并发安全特性

test "属性集合并发访问测试" {
  // 创建共享属性集合
  let shared_attrs = azimuth::Attributes {
    values: [
      ("service.name", azimuth::AttributeValue::StringValue("payment-service")),
      ("service.version", azimuth::AttributeValue::StringValue("2.1.0")),
      ("service.port", azimuth::AttributeValue::IntValue(8080))
    ]
  }
  
  // 模拟并发读取操作
  let read_attrs_1 = shared_attrs
  let read_attrs_2 = shared_attrs
  let read_attrs_3 = shared_attrs
  
  // 验证并发读取结果的一致性
  assert_eq(read_attrs_1.values.length(), 3)
  assert_eq(read_attrs_2.values.length(), 3)
  assert_eq(read_attrs_3.values.length(), 3)
  
  // 验证每个读取操作都能访问所有属性
  for attrs in [read_attrs_1, read_attrs_2, read_attrs_3] {
    let mut found_service_name = false
    let mut found_service_version = false
    let mut found_service_port = false
    
    for (key, value) in attrs.values {
      match key {
        "service.name" => {
          found_service_name = true
          match value {
            azimuth::AttributeValue::StringValue(name) => assert_eq(name, "payment-service")
            _ => assert_true(false)
          }
        }
        "service.version" => {
          found_service_version = true
          match value {
            azimuth::AttributeValue::StringValue(version) => assert_eq(version, "2.1.0")
            _ => assert_true(false)
          }
        }
        "service.port" => {
          found_service_port = true
          match value {
            azimuth::AttributeValue::IntValue(port) => assert_eq(port, 8080)
            _ => assert_true(false)
          }
        }
        _ => assert_true(false)
      }
    }
    
    assert_true(found_service_name)
    assert_true(found_service_version)
    assert_true(found_service_port)
  }
}

test "上下文并发传播测试" {
  // 创建共享上下文
  let shared_context = azimuth::Context {
    data: Some(("trace.id", "trace-12345"))
  }
  
  // 模拟多个并发操作访问同一上下文
  let context_1 = shared_context
  let context_2 = shared_context
  let context_3 = shared_context
  
  // 验证所有并发访问都能获取相同的上下文数据
  for context in [context_1, context_2, context_3] {
    match context.data {
      Some((key, value)) => {
        assert_eq(key, "trace.id")
        assert_eq(value, "trace-12345")
      }
      None => assert_true(false)
    }
  }
  
  // 模拟上下文传播
  let propagated_context_1 = azimuth::Context {
    data: Some(("trace.id", "trace-12345"))
  }
  
  let propagated_context_2 = azimuth::Context {
    data: Some(("trace.id", "trace-12345"))
  }
  
  // 验证传播的上下文保持一致性
  match propagated_context_1.data {
    Some((key1, value1)) => {
      match propagated_context_2.data {
        Some((key2, value2)) => {
          assert_eq(key1, key2)
          assert_eq(value1, value2)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

test "跨度上下文并发创建测试" {
  // 创建共享追踪ID
  let shared_trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  
  // 模拟并发创建多个跨度上下文
  let span_context_1 = azimuth::SpanContext {
    trace_id: shared_trace_id,
    span_id: "span-1",
    sampled: true,
    trace_state: ""
  }
  
  let span_context_2 = azimuth::SpanContext {
    trace_id: shared_trace_id,
    span_id: "span-2",
    sampled: true,
    trace_state: ""
  }
  
  let span_context_3 = azimuth::SpanContext {
    trace_id: shared_trace_id,
    span_id: "span-3",
    sampled: true,
    trace_state: ""
  }
  
  // 验证所有跨度上下文共享相同的追踪ID
  assert_eq(span_context_1.trace_id, shared_trace_id)
  assert_eq(span_context_2.trace_id, shared_trace_id)
  assert_eq(span_context_3.trace_id, shared_trace_id)
  
  // 验证每个跨度有唯一的跨度ID
  assert_ne(span_context_1.span_id, span_context_2.span_id)
  assert_ne(span_context_2.span_id, span_context_3.span_id)
  assert_ne(span_context_1.span_id, span_context_3.span_id)
  
  // 验证所有跨度都被采样
  assert_true(span_context_1.sampled)
  assert_true(span_context_2.sampled)
  assert_true(span_context_3.sampled)
}

test "行李并发操作测试" {
  // 创建初始行李
  let initial_baggage = azimuth::Baggage {
    entries: [
      ("user.id", "12345"),
      ("session.id", "session-abcde")
    ]
  }
  
  // 模拟并发添加操作
  let baggage_1 = azimuth::Baggage {
    entries: initial_baggage.entries + [
      ("request.id", "req-001"),
      ("operation.name", "process-payment")
    ]
  }
  
  let baggage_2 = azimuth::Baggage {
    entries: initial_baggage.entries + [
      ("request.id", "req-002"),
      ("operation.name", "validate-user")
    ]
  }
  
  let baggage_3 = azimuth::Baggage {
    entries: initial_baggage.entries + [
      ("request.id", "req-003"),
      ("operation.name", "send-notification")
    ]
  }
  
  // 验证每个行李都包含原始条目
  for baggage in [baggage_1, baggage_2, baggage_3] {
    let mut found_user_id = false
    let mut found_session_id = false
    
    for (key, value) in baggage.entries {
      match key {
        "user.id" => {
          found_user_id = true
          assert_eq(value, "12345")
        }
        "session.id" => {
          found_session_id = true
          assert_eq(value, "session-abcde")
        }
        _ => () // 其他条目
      }
    }
    
    assert_true(found_user_id)
    assert_true(found_session_id)
  }
  
  // 验证每个行李有不同的请求ID和操作名称
  let mut request_ids = []
  let mut operation_names = []
  
  for baggage in [baggage_1, baggage_2, baggage_3] {
    for (key, value) in baggage.entries {
      match key {
        "request.id" => request_ids = request_ids + [value]
        "operation.name" => operation_names = operation_names + [value]
        _ => () // 其他条目
      }
    }
  }
  
  assert_eq(request_ids.length(), 3)
  assert_eq(operation_names.length(), 3)
  
  // 验证请求ID和操作名称各不相同
  assert_ne(request_ids[0], request_ids[1])
  assert_ne(request_ids[1], request_ids[2])
  assert_ne(request_ids[0], request_ids[2])
  
  assert_ne(operation_names[0], operation_names[1])
  assert_ne(operation_names[1], operation_names[2])
  assert_ne(operation_names[0], operation_names[2])
}

test "资源并发合并测试" {
  // 创建基础资源
  let base_resource = azimuth::Resource {
    attributes: [
      ("service.name", azimuth::AttributeValue::StringValue("order-service")),
      ("service.version", azimuth::AttributeValue::StringValue("1.0.0"))
    ]
  }
  
  // 模拟并发合并操作
  let resource_1 = azimuth::Resource {
    attributes: base_resource.attributes + [
      ("host.name", azimuth::AttributeValue::StringValue("host-001")),
      ("deployment.environment", azimuth::AttributeValue::StringValue("staging"))
    ]
  }
  
  let resource_2 = azimuth::Resource {
    attributes: base_resource.attributes + [
      ("host.name", azimuth::AttributeValue::StringValue("host-002")),
      ("cloud.provider", azimuth::AttributeValue::StringValue("aws"))
    ]
  }
  
  let resource_3 = azimuth::Resource {
    attributes: base_resource.attributes + [
      ("host.name", azimuth::AttributeValue::StringValue("host-003")),
      ("region", azimuth::AttributeValue::StringValue("us-west-2"))
    ]
  }
  
  // 验证所有资源都包含基础属性
  for resource in [resource_1, resource_2, resource_3] {
    let mut found_service_name = false
    let mut found_service_version = false
    
    for (key, value) in resource.attributes {
      match key {
        "service.name" => {
          found_service_name = true
          match value {
            azimuth::AttributeValue::StringValue(name) => assert_eq(name, "order-service")
            _ => assert_true(false)
          }
        }
        "service.version" => {
          found_service_version = true
          match value {
            azimuth::AttributeValue::StringValue(version) => assert_eq(version, "1.0.0")
            _ => assert_true(false)
          }
        }
        _ => () // 其他属性
      }
    }
    
    assert_true(found_service_name)
    assert_true(found_service_version)
  }
  
  // 验证每个资源有不同的主机名
  let mut host_names = []
  for resource in [resource_1, resource_2, resource_3] {
    for (key, value) in resource.attributes {
      if key == "host.name" {
        match value {
          azimuth::AttributeValue::StringValue(host) => host_names = host_names + [host]
          _ => assert_true(false)
        }
      }
    }
  }
  
  assert_eq(host_names.length(), 3)
  assert_ne(host_names[0], host_names[1])
  assert_ne(host_names[1], host_names[2])
  assert_ne(host_names[0], host_names[2])
}

test "载体并发注入提取测试" {
  // 创建共享载体
  let base_carrier = azimuth::TextMapCarrier {
    headers: [
      ("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"),
      ("tracestate", "rojo=00f067aa0ba902b7")
    ]
  }
  
  // 模拟并发注入操作
  let carrier_1 = azimuth::TextMapCarrier {
    headers: base_carrier.headers + [
      ("x-request-id", "req-001"),
      ("x-operation-name", "process-payment")
    ]
  }
  
  let carrier_2 = azimuth::TextMapCarrier {
    headers: base_carrier.headers + [
      ("x-request-id", "req-002"),
      ("x-operation-name", "validate-user")
    ]
  }
  
  let carrier_3 = azimuth::TextMapCarrier {
    headers: base_carrier.headers + [
      ("x-request-id", "req-003"),
      ("x-operation-name", "send-notification")
    ]
  }
  
  // 验证所有载体都包含基础头
  for carrier in [carrier_1, carrier_2, carrier_3] {
    let mut found_traceparent = false
    let mut found_tracestate = false
    
    for (key, value) in carrier.headers {
      match key {
        "traceparent" => {
          found_traceparent = true
          assert_eq(value, "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")
        }
        "tracestate" => {
          found_tracestate = true
          assert_eq(value, "rojo=00f067aa0ba902b7")
        }
        _ => () // 其他头
      }
    }
    
    assert_true(found_traceparent)
    assert_true(found_tracestate)
  }
  
  // 验证每个载体有不同的请求ID和操作名称
  let mut request_ids = []
  let mut operation_names = []
  
  for carrier in [carrier_1, carrier_2, carrier_3] {
    for (key, value) in carrier.headers {
      match key {
        "x-request-id" => request_ids = request_ids + [value]
        "x-operation-name" => operation_names = operation_names + [value]
        _ => () // 其他头
      }
    }
  }
  
  assert_eq(request_ids.length(), 3)
  assert_eq(operation_names.length(), 3)
  
  // 验证请求ID和操作名称各不相同
  assert_ne(request_ids[0], request_ids[1])
  assert_ne(request_ids[1], request_ids[2])
  assert_ne(request_ids[0], request_ids[2])
  
  assert_ne(operation_names[0], operation_names[1])
  assert_ne(operation_names[1], operation_names[2])
  assert_ne(operation_names[0], operation_names[2])
}

test "属性值并发修改测试" {
  // 创建共享属性值
  let shared_string_value = azimuth::AttributeValue::StringValue("initial-value")
  let shared_int_value = azimuth::AttributeValue::IntValue(100)
  let shared_bool_value = azimuth::AttributeValue::BoolValue(true)
  
  // 模拟并发读取操作
  let string_value_1 = shared_string_value
  let string_value_2 = shared_string_value
  let string_value_3 = shared_string_value
  
  let int_value_1 = shared_int_value
  let int_value_2 = shared_int_value
  let int_value_3 = shared_int_value
  
  let bool_value_1 = shared_bool_value
  let bool_value_2 = shared_bool_value
  let bool_value_3 = shared_bool_value
  
  // 验证并发读取的一致性
  for string_value in [string_value_1, string_value_2, string_value_3] {
    match string_value {
      azimuth::AttributeValue::StringValue(s) => assert_eq(s, "initial-value")
      _ => assert_true(false)
    }
  }
  
  for int_value in [int_value_1, int_value_2, int_value_3] {
    match int_value {
      azimuth::AttributeValue::IntValue(i) => assert_eq(i, 100)
      _ => assert_true(false)
    }
  }
  
  for bool_value in [bool_value_1, bool_value_2, bool_value_3] {
    match bool_value {
      azimuth::AttributeValue::BoolValue(b) => assert_true(b)
      _ => assert_true(false)
    }
  }
  
  // 模拟并发创建新值（不可变模式）
  let new_string_value_1 = azimuth::AttributeValue::StringValue("modified-value-1")
  let new_string_value_2 = azimuth::AttributeValue::StringValue("modified-value-2")
  let new_string_value_3 = azimuth::AttributeValue::StringValue("modified-value-3")
  
  // 验证新值的独立性
  match new_string_value_1 {
    azimuth::AttributeValue::StringValue(s) => assert_eq(s, "modified-value-1")
    _ => assert_true(false)
  }
  
  match new_string_value_2 {
    azimuth::AttributeValue::StringValue(s) => assert_eq(s, "modified-value-2")
    _ => assert_true(false)
  }
  
  match new_string_value_3 {
    azimuth::AttributeValue::StringValue(s) => assert_eq(s, "modified-value-3")
    _ => assert_true(false)
  }
  
  // 验证原始值未被修改
  match shared_string_value {
    azimuth::AttributeValue::StringValue(s) => assert_eq(s, "initial-value")
    _ => assert_true(false)
  }
}

test "检测范围并发创建测试" {
  // 创建共享检测范围名称
  let shared_scope_name = "azimuth.tracer"
  
  // 模拟并发创建检测范围
  let scope_1 = azimuth::InstrumentationScope {
    name: shared_scope_name,
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  let scope_2 = azimuth::InstrumentationScope {
    name: shared_scope_name,
    version: Some("1.1.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.21.0")
  }
  
  let scope_3 = azimuth::InstrumentationScope {
    name: shared_scope_name,
    version: Some("1.2.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.22.0")
  }
  
  // 验证所有检测范围共享相同的名称
  assert_eq(scope_1.name, shared_scope_name)
  assert_eq(scope_2.name, shared_scope_name)
  assert_eq(scope_3.name, shared_scope_name)
  
  // 验证每个检测范围有不同的版本和schema URL
  match scope_1.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => assert_true(false)
  }
  
  match scope_2.version {
    Some(v) => assert_eq(v, "1.1.0")
    None => assert_true(false)
  }
  
  match scope_3.version {
    Some(v) => assert_eq(v, "1.2.0")
    None => assert_true(false)
  }
  
  match scope_1.schema_url {
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.20.0")
    None => assert_true(false)
  }
  
  match scope_2.schema_url {
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.21.0")
    None => assert_true(false)
  }
  
  match scope_3.schema_url {
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.22.0")
    None => assert_true(false)
  }
}

test "并发数据完整性测试" {
  // 创建复杂数据结构
  let complex_data = azimuth::Resource {
    attributes: [
      ("service.name", azimuth::AttributeValue::StringValue("complex-service")),
      ("service.version", azimuth::AttributeValue::StringValue("2.5.0")),
      ("service.tags", azimuth::AttributeValue::ArrayStringValue(["api", "payment", "critical"])),
      ("service.config", azimuth::AttributeValue::ArrayIntValue([8080, 8443, 9090])),
      ("service.features", azimuth::AttributeValue::BoolValue(true)),
      ("service.weight", azimuth::AttributeValue::FloatValue(0.95))
    ]
  }
  
  // 模拟多个并发操作访问复杂数据
  let data_copy_1 = complex_data
  let data_copy_2 = complex_data
  let data_copy_3 = complex_data
  
  // 验证每个副本的数据完整性
  for data_copy in [data_copy_1, data_copy_2, data_copy_3] {
    assert_eq(data_copy.attributes.length(), 6)
    
    // 验证每种类型的属性
    let mut found_string = false
    let mut found_array_string = false
    let mut found_array_int = false
    let mut found_bool = false
    let mut found_float = false
    
    for (key, value) in data_copy.attributes {
      match key {
        "service.name" => {
          found_string = true
          match value {
            azimuth::AttributeValue::StringValue(s) => assert_eq(s, "complex-service")
            _ => assert_true(false)
          }
        }
        "service.tags" => {
          found_array_string = true
          match value {
            azimuth::AttributeValue::ArrayStringValue(arr) => {
              assert_eq(arr.length(), 3)
              assert_eq(arr[0], "api")
              assert_eq(arr[1], "payment")
              assert_eq(arr[2], "critical")
            }
            _ => assert_true(false)
          }
        }
        "service.config" => {
          found_array_int = true
          match value {
            azimuth::AttributeValue::ArrayIntValue(arr) => {
              assert_eq(arr.length(), 3)
              assert_eq(arr[0], 8080)
              assert_eq(arr[1], 8443)
              assert_eq(arr[2], 9090)
            }
            _ => assert_true(false)
          }
        }
        "service.features" => {
          found_bool = true
          match value {
            azimuth::AttributeValue::BoolValue(b) => assert_true(b)
            _ => assert_true(false)
          }
        }
        "service.weight" => {
          found_float = true
          match value {
            azimuth::AttributeValue::FloatValue(f) => assert_true(f > 0.94 && f < 0.96)
            _ => assert_true(false)
          }
        }
        _ => () // 其他属性
      }
    }
    
    assert_true(found_string)
    assert_true(found_array_string)
    assert_true(found_array_int)
    assert_true(found_bool)
    assert_true(found_float)
  }
}