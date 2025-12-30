// 分布式追踪高级测试用例
// 测试Azimuth Telemetry分布式追踪功能

test "distributed_trace_context_propagation" {
  // 测试分布式追踪上下文传播
  
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let parent_span_id = "00f067aa0ba902b7"
  let span_id = "b7ad6b7169203331"
  let flags = "01"
  
  // 创建traceparent header
  let traceparent = "00-" + trace_id + "-" + parent_span_id + "-" + flags
  assert_eq(traceparent.length(), 55)
  assert_eq(traceparent.has_prefix("00-"), true)
  assert_eq(traceparent.contains(trace_id), true)
  assert_eq(traceparent.contains(parent_span_id), true)
  assert_eq(traceparent.has_suffix("-" + flags), true)
  
  // 验证trace_id格式
  assert_eq(trace_id.length(), 32)
  assert_eq(trace_id.has_prefix("4bf9"), true)
  assert_eq(trace_id.has_suffix("4736"), true)
  
  // 验证span_id格式
  assert_eq(parent_span_id.length(), 16)
  assert_eq(span_id.length(), 16)
  assert_eq(parent_span_id.has_prefix("00f0"), true)
  assert_eq(span_id.has_prefix("b7ad"), true)
  
  // 创建tracestate header
  let tracestate = "vendor1=opaqueValue1,vendor2=opaqueValue2,custom=value3"
  assert_eq(tracestate.length(), 52)
  assert_eq(tracestate.contains("vendor1"), true)
  assert_eq(tracestate.contains("vendor2"), true)
  assert_eq(tracestate.contains("custom"), true)
  
  // 验证tracestate格式
  let tracestate_parts = tracestate.split(",")
  assert_eq(tracestate_parts.length(), 3)
  assert_eq(tracestate_parts[0], "vendor1=opaqueValue1")
  assert_eq(tracestate_parts[2], "custom=value3")
}

test "span_hierarchy_and_relationships" {
  // 测试span层次结构和关系
  
  // 创建根span
  let root_span = {
    trace_id: "a1b2c3d4e5f678901234567890123456",
    span_id: "1111111111111111",
    parent_span_id: None,
    operation_name: "http.request",
    start_time: 1640995200000L,
    end_time: 1640995200250L,
    status: "ok",
    attributes: [
      ("http.method", "GET"),
      ("http.url", "/api/users"),
      ("http.status_code", "200")
    ]
  }
  
  // 创建子span
  let child_span = {
    trace_id: root_span.trace_id,
    span_id: "2222222222222222",
    parent_span_id: Some(root_span.span_id),
    operation_name: "database.query",
    start_time: 1640995200050L,
    end_time: 1640995200200L,
    status: "ok",
    attributes: [
      ("db.statement", "SELECT * FROM users"),
      ("db.type", "postgresql"),
      ("db.rows_affected", "10")
    ]
  }
  
  // 创建孙子span
  let grandchild_span = {
    trace_id: root_span.trace_id,
    span_id: "3333333333333333",
    parent_span_id: Some(child_span.span_id),
    operation_name: "cache.get",
    start_time: 1640995200100L,
    end_time: 1640995200150L,
    status: "miss",
    attributes: [
      ("cache.key", "user:123"),
      ("cache.hit", "false")
    ]
  }
  
  // 验证span关系
  assert_eq(root_span.trace_id, child_span.trace_id)
  assert_eq(child_span.trace_id, grandchild_span.trace_id)
  
  match child_span.parent_span_id {
    Some(parent) => assert_eq(parent, root_span.span_id)
    None => assert_eq(false, true, "Expected parent_span_id to be Some")
  }
  
  match grandchild_span.parent_span_id {
    Some(parent) => assert_eq(parent, child_span.span_id)
    None => assert_eq(false, true, "Expected parent_span_id to be Some")
  }
  
  // 验证时间顺序
  assert_eq(root_span.start_time < child_span.start_time, true)
  assert_eq(child_span.start_time < grandchild_span.start_time, true)
  assert_eq(grandchild_span.end_time < child_span.end_time, true)
  assert_eq(child_span.end_time < root_span.end_time, true)
  
  // 验证持续时间
  let root_duration = root_span.end_time - root_span.start_time
  let child_duration = child_span.end_time - child_span.start_time
  let grandchild_duration = grandchild_span.end_time - grandchild_span.start_time
  
  assert_eq(root_duration, 250L)
  assert_eq(child_duration, 150L)
  assert_eq(grandchild_duration, 50L)
  assert_eq(root_duration > child_duration, true)
  assert_eq(child_duration > grandchild_duration, true)
}

test "span_events_and_annotations" {
  // 测试span事件和注解
  
  let span_events = [
    {
      name: "exception",
      timestamp: 1640995200100L,
      attributes: [
        ("exception.type", "TimeoutException"),
        ("exception.message", "Request timeout after 30 seconds"),
        ("exception.stacktrace", "at com.example.Service.process(Service.java:123)")
      ]
    },
    {
      name: "log",
      timestamp: 1640995200150L,
      attributes: [
        ("log.level", "warn"),
        ("log.message", "Retrying request after timeout"),
        ("retry.attempt", "1")
      ]
    },
    {
      name: "message",
      timestamp: 1640995200200L,
      attributes: [
        ("message.id", "msg-12345"),
        ("message.type", "request"),
        ("message.size", "1024")
      ]
    }
  ]
  
  assert_eq(span_events.length(), 3)
  
  // 验证异常事件
  let exception_event = span_events[0]
  assert_eq(exception_event.name, "exception")
  assert_eq(exception_event.attributes.length(), 3)
  
  let (exception_type_key, exception_type_value) = exception_event.attributes[0]
  assert_eq(exception_type_key, "exception.type")
  assert_eq(exception_type_value, "TimeoutException")
  
  // 验证日志事件
  let log_event = span_events[1]
  assert_eq(log_event.name, "log")
  assert_eq(log_event.attributes.length(), 3)
  
  let (log_level_key, log_level_value) = log_event.attributes[0]
  assert_eq(log_level_key, "log.level")
  assert_eq(log_level_value, "warn")
  
  // 验证消息事件
  let message_event = span_events[2]
  assert_eq(message_event.name, "message")
  assert_eq(message_event.attributes.length(), 3)
  
  let (message_id_key, message_id_value) = message_event.attributes[0]
  assert_eq(message_id_key, "message.id")
  assert_eq(message_id_value, "msg-12345")
  
  // 验证事件时间顺序
  assert_eq(exception_event.timestamp < log_event.timestamp, true)
  assert_eq(log_event.timestamp < message_event.timestamp, true)
}

test "span_links_and_cross_trace_correlation" {
  // 测试span链接和跨追踪关联
  
  let current_trace_id = "1234567890abcdef1234567890abcdef"
  let current_span_id = "1111111111111111"
  
  // 创建span链接
  let span_links = [
    {
      trace_id: "fedcba0987654321fedcba0987654321",
      span_id: "2222222222222222",
      trace_state: "vendor1=value1,vendor2=value2",
      attributes: [
        ("link.type", "follows_from"),
        ("link.reason", "async_dependency")
      ]
    },
    {
      trace_id: "abcdef1234567890abcdef1234567890",
      span_id: "3333333333333333",
      trace_state: "custom=custom_value",
      attributes: [
        ("link.type", "child_of"),
        ("link.reason", "batch_processing")
      ]
    }
  ]
  
  assert_eq(span_links.length(), 2)
  
  // 验证第一个链接
  let link1 = span_links[0]
  assert_eq(link1.trace_id, "fedcba0987654321fedcba0987654321")
  assert_eq(link1.span_id, "2222222222222222")
  assert_eq(link1.trace_state.contains("vendor1"), true)
  assert_eq(link1.attributes.length(), 2)
  
  let (link1_type_key, link1_type_value) = link1.attributes[0]
  assert_eq(link1_type_key, "link.type")
  assert_eq(link1_type_value, "follows_from")
  
  // 验证第二个链接
  let link2 = span_links[1]
  assert_eq(link2.trace_id, "abcdef1234567890abcdef1234567890")
  assert_eq(link2.span_id, "3333333333333333")
  assert_eq(link2.trace_state.contains("custom"), true)
  
  let (link2_reason_key, link2_reason_value) = link2.attributes[1]
  assert_eq(link2_reason_key, "link.reason")
  assert_eq(link2_reason_value, "batch_processing")
  
  // 验证链接的trace_id与当前trace_id不同
  assert_eq(link1.trace_id != current_trace_id, true)
  assert_eq(link2.trace_id != current_trace_id, true)
  assert_eq(link1.trace_id != link2.trace_id, true)
}

test "span_status_and_error_handling" {
  // 测试span状态和错误处理
  
  // 创建不同状态的span
  let ok_span = {
    span_id: "1111111111111111",
    operation_name: "successful.operation",
    status: "ok",
    status_code: 1,
    status_message: "Operation completed successfully"
  }
  
  let error_span = {
    span_id: "2222222222222222",
    operation_name: "failed.operation",
    status: "error",
    status_code: 2,
    status_message: "Database connection failed"
  }
  
  let timeout_span = {
    span_id: "3333333333333333",
    operation_name: "timeout.operation",
    status: "timeout",
    status_code: 3,
    status_message: "Request timed out after 30 seconds"
  }
  
  let cancelled_span = {
    span_id: "4444444444444444",
    operation_name: "cancelled.operation",
    status: "cancelled",
    status_code: 4,
    status_message: "Operation was cancelled by client"
  }
  
  // 验证状态
  assert_eq(ok_span.status, "ok")
  assert_eq(ok_span.status_code, 1)
  assert_eq(ok_span.status_message.contains("successfully"), true)
  
  assert_eq(error_span.status, "error")
  assert_eq(error_span.status_code, 2)
  assert_eq(error_span.status_message.contains("failed"), true)
  
  assert_eq(timeout_span.status, "timeout")
  assert_eq(timeout_span.status_code, 3)
  assert_eq(timeout_span.status_message.contains("timed out"), true)
  
  assert_eq(cancelled_span.status, "cancelled")
  assert_eq(cancelled_span.status_code, 4)
  assert_eq(cancelled_span.status_message.contains("cancelled"), true)
  
  // 创建状态数组进行验证
  let spans = [ok_span, error_span, timeout_span, cancelled_span]
  assert_eq(spans.length(), 4)
  
  // 验证成功span数量
  let mut success_count = 0
  let mut error_count = 0
  let mut i = 0
  while i < spans.length() {
    if spans[i].status == "ok" {
      success_count = success_count + 1
    } else {
      error_count = error_count + 1
    }
    i = i + 1
  }
  
  assert_eq(success_count, 1)
  assert_eq(error_count, 3)
}

test "baggage_items_propagation" {
  // 测试baggage项传播
  
  // 创建baggage项
  let baggage_items = [
    ("user.id", "12345"),
    ("request.id", "req-67890"),
    ("session.id", "sess-abc123"),
    ("tenant.id", "tenant-def456"),
    ("correlation.id", "corr-789xyz")
  ]
  
  assert_eq(baggage_items.length(), 5)
  
  // 验证baggage项格式
  let mut i = 0
  while i < baggage_items.length() {
    let (key, value) = baggage_items[i]
    assert_eq(key.contains("."), true)
    assert_eq(value.length() > 0, true)
    i = i + 1
  }
  
  // 创建baggage header字符串
  let baggage_header = ""
  let mut j = 0
  while j < baggage_items.length() {
    let (key, value) = baggage_items[j]
    if j > 0 {
      baggage_header = baggage_header + ","
    }
    baggage_header = baggage_header + key + "=" + value
    j = j + 1
  }
  
  // 验证baggage header
  assert_eq(baggage_header.contains("user.id=12345"), true)
  assert_eq(baggage_header.contains("request.id=req-67890"), true)
  assert_eq(baggage_header.contains("session.id=sess-abc123"), true)
  assert_eq(baggage_header.contains("tenant.id=tenant-def456"), true)
  assert_eq(baggage_header.contains("correlation.id=corr-789xyz"), true)
  
  // 验证baggage项数量
  let header_parts = baggage_header.split(",")
  assert_eq(header_parts.length(), 5)
  
  // 验证特定baggage项
  assert_eq(header_parts[0], "user.id=12345")
  assert_eq(header_parts[2], "session.id=sess-abc123")
  assert_eq(header_parts[4], "correlation.id=corr-789xyz")
}