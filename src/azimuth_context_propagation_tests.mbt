// Azimuth Context Propagation Test Suite
// 测试上下文传播功能，包括跨服务、跨进程的追踪上下文传递

test "基本上下文创建和访问" {
  // 创建基本上下文
  let context = azimuth::Context {
    data: Some(("trace.id", "1234567890abcdef"))
  }
  
  // 验证上下文数据
  match context.data {
    Some((key, value)) => {
      assert_eq(key, "trace.id")
      assert_eq(value, "1234567890abcdef")
    }
    None => assert_true(false)
  }
  
  // 创建空上下文
  let empty_context = azimuth::Context { data: None }
  assert_eq(empty_context.data, None)
}

test "上下文键操作测试" {
  // 创建不同类型的上下文键
  let trace_key = azimuth::ContextKey[String] { key: "trace.context" }
  let user_key = azimuth::ContextKey[String] { key: "user.context" }
  let request_key = azimuth::ContextKey[String] { key: "request.context" }
  
  // 验证键名
  assert_eq(trace_key.key, "trace.context")
  assert_eq(user_key.key, "user.context")
  assert_eq(request_key.key, "request.context")
  
  // 验证键的唯一性
  assert_true(trace_key.key != user_key.key)
  assert_true(user_key.key != request_key.key)
  assert_true(trace_key.key != request_key.key)
}

test "上下文数据存储和检索" {
  // 创建上下文
  let mut context = azimuth::Context { data: None }
  
  // 存储数据（模拟设置操作）
  context.data = Some(("user.id", "user-12345"))
  
  // 检索数据
  match context.data {
    Some((key, value)) => {
      assert_eq(key, "user.id")
      assert_eq(value, "user-12345")
    }
    None => assert_true(false)
  }
  
  // 更新数据
  context.data = Some(("user.id", "user-67890"))
  
  // 验证更新后的数据
  match context.data {
    Some((key, value)) => {
      assert_eq(key, "user.id")
      assert_eq(value, "user-67890")
    }
    None => assert_true(false)
  }
}

test "行李(Baggage)基本操作" {
  // 创建空行李
  let empty_baggage = azimuth::Baggage { entries: [] }
  assert_eq(empty_baggage.entries.length(), 0)
  
  // 创建包含条目的行李
  let baggage = azimuth::Baggage {
    entries: [
      ("user.id", "12345"),
      ("session.id", "session-abcde"),
      ("request.id", "req-67890"),
      ("correlation.id", "corr-12345")
    ]
  }
  
  assert_eq(baggage.entries.length(), 4)
  
  // 验证行李条目
  let mut found_user_id = false
  let mut found_session_id = false
  let mut found_request_id = false
  let mut found_correlation_id = false
  
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
      "request.id" => {
        found_request_id = true
        assert_eq(value, "req-67890")
      }
      "correlation.id" => {
        found_correlation_id = true
        assert_eq(value, "corr-12345")
      }
      _ => assert_true(false)
    }
  }
  
  assert_true(found_user_id)
  assert_true(found_session_id)
  assert_true(found_request_id)
  assert_true(found_correlation_id)
}

test "行李条目添加和删除" {
  // 创建初始行李
  let mut baggage = azimuth::Baggage {
    entries: [
      ("user.id", "12345"),
      ("session.id", "session-abcde")
    ]
  }
  
  assert_eq(baggage.entries.length(), 2)
  
  // 添加新条目（模拟添加操作）
  baggage.entries = baggage.entries + [("request.id", "req-67890")]
  assert_eq(baggage.entries.length(), 3)
  
  // 验证新添加的条目
  let mut found_request_id = false
  for (key, value) in baggage.entries {
    if key == "request.id" {
      found_request_id = true
      assert_eq(value, "req-67890")
    }
  }
  assert_true(found_request_id)
  
  // 删除条目（模拟删除操作）
  let mut new_entries = []
  for (key, value) in baggage.entries {
    if key != "session.id" {
      new_entries = new_entries + [(key, value)]
    }
  }
  baggage.entries = new_entries
  
  assert_eq(baggage.entries.length(), 2)
  
  // 验证删除后的条目
  let mut found_session_id = false
  for (key, _) in baggage.entries {
    if key == "session.id" {
      found_session_id = true
      break
    }
  }
  assert_false(found_session_id)
}

test "跨度上下文(SpanContext)基本操作" {
  // 创建跨度上下文
  let span_context = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    sampled: true,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 验证跨度上下文属性
  assert_eq(span_context.trace_id, "4bf92f3577b34da6a3ce929d0e0e4736")
  assert_eq(span_context.span_id, "00f067aa0ba902b7")
  assert_true(span_context.sampled)
  assert_eq(span_context.trace_state, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  
  // 创建未采样的跨度上下文
  let unsampled_context = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    sampled: false,
    trace_state: ""
  }
  
  assert_false(unsampled_context.sampled)
  assert_eq(unsampled_context.trace_state, "")
}

test "文本映射载体(TextMapCarrier)操作" {
  // 创建空载体
  let empty_carrier = azimuth::TextMapCarrier { headers: [] }
  assert_eq(empty_carrier.headers.length(), 0)
  
  // 创建包含追踪头的载体
  let carrier = azimuth::TextMapCarrier {
    headers: [
      ("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"),
      ("tracestate", "rojo=00f067aa0ba902b7"),
      ("x-correlation-id", "corr-12345"),
      ("x-request-id", "req-67890")
    ]
  }
  
  assert_eq(carrier.headers.length(), 4)
  
  // 验证追踪头
  let mut found_traceparent = false
  let mut found_tracestate = false
  let mut found_correlation_id = false
  let mut found_request_id = false
  
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
      "x-correlation-id" => {
        found_correlation_id = true
        assert_eq(value, "corr-12345")
      }
      "x-request-id" => {
        found_request_id = true
        assert_eq(value, "req-67890")
      }
      _ => assert_true(false)
    }
  }
  
  assert_true(found_traceparent)
  assert_true(found_tracestate)
  assert_true(found_correlation_id)
  assert_true(found_request_id)
}

test "上下文传播模拟" {
  // 模拟服务A创建上下文
  let service_a_context = azimuth::Context {
    data: Some(("trace.id", "trace-service-a-123"))
  }
  
  let service_a_baggage = azimuth::Baggage {
    entries: [
      ("service.name", "service-a"),
      ("user.id", "user-12345"),
      ("request.id", "req-service-a-001")
    ]
  }
  
  // 模拟服务B接收上下文
  let service_b_context = service_a_context
  let mut service_b_baggage = service_a_baggage
  
  // 服务B添加自己的信息
  service_b_baggage.entries = service_b_baggage.entries + [
    ("service.name", "service-b"),
    ("operation.name", "process-data"),
    ("start.time", "2023-01-01T00:00:00Z")
  ]
  
  // 验证服务B的上下文
  match service_b_context.data {
    Some((key, value)) => {
      assert_eq(key, "trace.id")
      assert_eq(value, "trace-service-a-123")
    }
    None => assert_true(false)
  }
  
  // 验证服务B的行李包含所有信息
  assert_eq(service_b_baggage.entries.length(), 6)
  
  // 验证原始信息仍然存在
  let mut found_user_id = false
  let mut found_request_id = false
  let mut found_service_b = false
  let mut found_operation = false
  
  for (key, value) in service_b_baggage.entries {
    match key {
      "user.id" => {
        found_user_id = true
        assert_eq(value, "user-12345")
      }
      "request.id" => {
        found_request_id = true
        assert_eq(value, "req-service-a-001")
      }
      "service.name" => {
        if value == "service-b" {
          found_service_b = true
        }
      }
      "operation.name" => {
        found_operation = true
        assert_eq(value, "process-data")
      }
      _ => () // 其他键值对
    }
  }
  
  assert_true(found_user_id)
  assert_true(found_request_id)
  assert_true(found_service_b)
  assert_true(found_operation)
}

test "跨进程上下文传播" {
  // 模拟进程A创建跨度上下文
  let process_a_span_context = azimuth::SpanContext {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331",
    sampled: true,
    trace_state: "rojo=00f067aa0ba902b7"
  }
  
  // 模拟将上下文注入到载体中
  let carrier = azimuth::TextMapCarrier {
    headers: [
      ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
      ("tracestate", "rojo=00f067aa0ba902b7"),
      ("x-process-id", "process-a-001")
    ]
  }
  
  // 模拟进程B从载体中提取上下文
  let mut extracted_trace_id = ""
  let mut extracted_span_id = ""
  let mut extracted_sampled = false
  let mut extracted_trace_state = ""
  
  for (key, value) in carrier.headers {
    match key {
      "traceparent" => {
        // 简化的解析逻辑
        // 格式: 00-trace_id-span_id-flags
        let parts = value.split("-")
        if parts.length() >= 4 {
          extracted_trace_id = parts[1]
          extracted_span_id = parts[2]
          extracted_sampled = parts[3] == "01"
        }
      }
      "tracestate" => {
        extracted_trace_state = value
      }
      _ => () // 其他头信息
    }
  }
  
  // 验证提取的上下文
  assert_eq(extracted_trace_id, "0af7651916cd43dd8448eb211c80319c")
  assert_eq(extracted_span_id, "b7ad6b7169203331")
  assert_true(extracted_sampled)
  assert_eq(extracted_trace_state, "rojo=00f067aa0ba902b7")
  
  // 创建进程B的跨度上下文
  let process_b_span_context = azimuth::SpanContext {
    trace_id: extracted_trace_id,
    span_id: "new-span-id-process-b", // 生成新的跨度ID
    sampled: extracted_sampled,
    trace_state: extracted_trace_state
  }
  
  // 验证进程B的跨度上下文
  assert_eq(process_b_span_context.trace_id, process_a_span_context.trace_id)
  assert_eq(process_b_span_context.sampled, process_a_span_context.sampled)
  assert_eq(process_b_span_context.trace_state, process_a_span_context.trace_state)
  assert_ne(process_b_span_context.span_id, process_a_span_context.span_id) // 应该有不同的跨度ID
}

test "上下文传播边界条件" {
  // 测试空上下文传播
  let empty_context = azimuth::Context { data: None }
  let propagated_empty_context = empty_context
  assert_eq(propagated_empty_context.data, None)
  
  // 测试空行李传播
  let empty_baggage = azimuth::Baggage { entries: [] }
  let propagated_empty_baggage = empty_baggage
  assert_eq(propagated_empty_baggage.entries.length(), 0)
  
  // 测试空载体传播
  let empty_carrier = azimuth::TextMapCarrier { headers: [] }
  let propagated_empty_carrier = empty_carrier
  assert_eq(propagated_empty_carrier.headers.length(), 0)
  
  // 测试包含特殊字符的上下文
  let special_context = azimuth::Context {
    data: Some(("special.key", "value with spaces & special chars!@#$%"))
  }
  
  match special_context.data {
    Some((key, value)) => {
      assert_eq(key, "special.key")
      assert_eq(value, "value with spaces & special chars!@#$%")
    }
    None => assert_true(false)
  }
  
  // 测试长字符串值的上下文
  let long_value = "a".repeat(1000) // 创建长字符串
  let long_context = azimuth::Context {
    data: Some(("long.key", long_value))
  }
  
  match long_context.data {
    Some((key, value)) => {
      assert_eq(key, "long.key")
      assert_eq(value.length(), 1000)
    }
    None => assert_true(false)
  }
}

test "上下文传播错误处理" {
  // 测试格式错误的traceparent
  let malformed_carrier = azimuth::TextMapCarrier {
    headers: [
      ("traceparent", "malformed-traceparent-value"),
      ("tracestate", "valid-tracestate-value")
    ]
  }
  
  // 模拟解析错误处理
  let mut parse_error = false
  for (key, value) in malformed_carrier.headers {
    if key == "traceparent" {
      // 简化的验证逻辑
      let parts = value.split("-")
      if parts.length() < 4 {
        parse_error = true
      }
    }
  }
  assert_true(parse_error)
  
  // 测试缺失关键头的载体
  let incomplete_carrier = azimuth::TextMapCarrier {
    headers: [
      ("x-custom-header", "custom-value")
      // 缺少traceparent和tracestate
    ]
  }
  
  // 模拟缺失头的处理
  let mut has_traceparent = false
  let mut has_tracestate = false
  
  for (key, _) in incomplete_carrier.headers {
    if key == "traceparent" {
      has_traceparent = true
    } else if key == "tracestate" {
      has_tracestate = true
    }
  }
  
  assert_false(has_traceparent)
  assert_false(has_tracestate)
  
  // 测试空值头
  let empty_value_carrier = azimuth::TextMapCarrier {
    headers: [
      ("traceparent", ""),
      ("tracestate", ""),
      ("x-empty-header", "")
    ]
  }
  
  // 验证空值处理
  for (key, value) in empty_value_carrier.headers {
    assert_eq(value, "")
  }
}