// Azimuth 分布式链路追踪测试
// 测试分布式链路追踪的核心功能

// 测试1: Span 创建和基本属性
test "Span 创建和基本属性测试" {
  // 创建 Span
  let span = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    parent_span_id: Some("b7ad6b7169203331"),
    name: "http GET /api/users",
    kind: SpanKind.Server,
    start_time: 1640995200000000000L, // 2022-01-01 00:00:00 UTC in nanos
    end_time: None,
    status: Status.Unset,
    attributes: [
      ("http.method", "GET"),
      ("http.url", "/api/users"),
      ("http.host", "api.example.com"),
      ("http.scheme", "https")
    ],
    events: [],
    links: []
  })
  
  // 验证 Span 基本属性
  assert_eq(span.trace_id, "4bf92f3577b34da6a3ce929d0e0e4736")
  assert_eq(span.span_id, "00f067aa0ba902b7")
  match span.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, "b7ad6b7169203331")
    None => assert_true(false)
  }
  assert_eq(span.name, "http GET /api/users")
  assert_eq(span.kind, SpanKind.Server)
  assert_eq(span.start_time, 1640995200000000000L)
  assert_eq(span.status, Status.Unset)
  
  // 验证 Span 属性
  assert_eq(span.attributes.length(), 4)
  assert_true(span.attributes.contains(("http.method", "GET")))
  assert_true(span.attributes.contains(("http.url", "/api/users")))
  assert_true(span.attributes.contains(("http.host", "api.example.com")))
  assert_true(span.attributes.contains(("http.scheme", "https")))
  
  // 验证初始状态
  assert_eq(span.events.length(), 0)
  assert_eq(span.links.length(), 0)
  assert_eq(span.end_time, None)
}

// 测试2: Span 生命周期管理
test "Span 生命周期管理测试" {
  // 创建 Span
  let mut span = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    parent_span_id: None,
    name: "database query",
    kind: SpanKind.Client,
    start_time: 1640995200000000000L,
    end_time: None,
    status: Status.Unset,
    attributes: [
      ("db.system", "postgresql"),
      ("db.statement", "SELECT * FROM users WHERE id = $1")
    ],
    events: [],
    links: []
  })
  
  // 添加事件
  span = { span with 
    events: span.events.push({
      name: "db.query.prepare",
      timestamp: 1640995200100000000L,
      attributes: [
        ("db.query.prepare.time_ms", "5")
      ]
    })
  }
  
  span = { span with 
    events: span.events.push({
      name: "db.query.execute",
      timestamp: 1640995200150000000L,
      attributes: [
        ("db.query.execute.time_ms", "25")
      ]
    })
  }
  
  // 验证事件
  assert_eq(span.events.length(), 2)
  assert_eq(span.events[0].name, "db.query.prepare")
  assert_eq(span.events[1].name, "db.query.execute")
  
  // 设置状态并结束 Span
  span = { span with 
    status: Status.Ok,
    end_time: Some(1640995200200000000L)
  }
  
  // 验证 Span 结束状态
  assert_eq(span.status, Status.Ok)
  match span.end_time {
    Some(end_time) => assert_eq(end_time, 1640995200200000000L)
    None => assert_true(false)
  }
  
  // 计算 Span 持续时间
  let duration = match span.end_time {
    Some(end_time) => end_time - span.start_time
    None => 0L
  }
  
  assert_eq(duration, 200000000L) // 200ms in nanos
}

// 测试3: Span 链和父子关系
test "Span 链和父子关系测试" {
  // 创建根 Span
  let root_span = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "b7ad6b7169203331",
    parent_span_id: None,
    name: "HTTP request",
    kind: SpanKind.Server,
    start_time: 1640995200000000000L,
    end_time: Some(1640995201000000000L),
    status: Status.Ok,
    attributes: [
      ("http.method", "GET"),
      ("http.url", "/api/orders")
    ],
    events: [],
    links: []
  })
  
  // 创建子 Span
  let child_span1 = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    parent_span_id: Some("b7ad6b7169203331"),
    name: "database query",
    kind: SpanKind.Client,
    start_time: 1640995200100000000L,
    end_time: Some(1640995200800000000L),
    status: Status.Ok,
    attributes: [
      ("db.system", "postgresql")
    ],
    events: [],
    links: []
  })
  
  let child_span2 = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "e457b5a2e4dcb0d6",
    parent_span_id: Some("b7ad6b7169203331"),
    name: "cache lookup",
    kind: SpanKind.Internal,
    start_time: 1640995200200000000L,
    end_time: Some(1640995200400000000L),
    status: Status.Ok,
    attributes: [
      ("cache.system", "redis")
    ],
    events: [],
    links: []
  })
  
  // 创建孙 Span
  let grandchild_span = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "a3f2c1d4e5b6a7b8",
    parent_span_id: Some("00f067aa0ba902b7"),
    name: "db connection",
    kind: SpanKind.Client,
    start_time: 1640995200150000000L,
    end_time: Some(1640995200250000000L),
    status: Status.Ok,
    attributes: [],
    events: [],
    links: []
  })
  
  // 验证父子关系
  assert_eq(root_span.parent_span_id, None) // 根 Span 没有父级
  match child_span1.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, root_span.span_id)
    None => assert_true(false)
  }
  match child_span2.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, root_span.span_id)
    None => assert_true(false)
  }
  match grandchild_span.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, child_span1.span_id)
    None => assert_true(false)
  }
  
  // 验证所有 Span 属于同一个 Trace
  assert_eq(root_span.trace_id, child_span1.trace_id)
  assert_eq(root_span.trace_id, child_span2.trace_id)
  assert_eq(root_span.trace_id, grandchild_span.trace_id)
  
  // 构建 Span 树
  let spans = [root_span, child_span1, child_span2, grandchild_span]
  
  // 找出根 Span
  let root_spans = spans.filter(fn(s) { s.parent_span_id == None })
  assert_eq(root_spans.length(), 1)
  assert_eq(root_spans[0].span_id, "b7ad6b7169203331")
  
  // 找出根 Span 的直接子 Span
  let root_children = spans.filter(fn(s) { 
    match s.parent_span_id {
      Some(parent_id) => parent_id == root_span.span_id
      None => false
    }
  })
  assert_eq(root_children.length(), 2)
  
  // 找出子 Span 的子 Span
  let child1_children = spans.filter(fn(s) { 
    match s.parent_span_id {
      Some(parent_id) => parent_id == child_span1.span_id
      None => false
    }
  })
  assert_eq(child1_children.length(), 1)
}

// 测试4: Span 上下文传播
test "Span 上下文传播测试" {
  // 创建初始 SpanContext
  let span_context = SpanContext({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "b7ad6b7169203331",
    trace_flags: 0x01, // 采样标志
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE",
    is_remote: false
  })
  
  // 验证 SpanContext
  assert_eq(span_context.trace_id, "4bf92f3577b34da6a3ce929d0e0e4736")
  assert_eq(span_context.span_id, "b7ad6b7169203331")
  assert_eq(span_context.trace_flags, 0x01)
  assert_eq(span_context.trace_state, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  assert_eq(span_context.is_remote, false)
  
  // 验证采样标志
  let is_sampled = (span_context.trace_flags & 0x01) != 0
  assert_true(is_sampled)
  
  // 创建新的 SpanContext（模拟跨服务传播）
  let remote_span_context = SpanContext({
    trace_id: span_context.trace_id, // 保持相同的 trace_id
    span_id: "00f067aa0ba902b7",     // 新的 span_id
    trace_flags: span_context.trace_flags, // 保持相同的 trace_flags
    trace_state: span_context.trace_state,  // 保持相同的 trace_state
    is_remote: true // 标记为远程
  })
  
  // 验证远程 SpanContext
  assert_eq(remote_span_context.trace_id, span_context.trace_id)
  assert_eq(remote_span_context.span_id, "00f067aa0ba902b7")
  assert_eq(remote_span_context.trace_flags, span_context.trace_flags)
  assert_eq(remote_span_context.trace_state, span_context.trace_state)
  assert_eq(remote_span_context.is_remote, true)
  
  // 验证 tracestate 格式
  let trace_state_entries = remote_span_context.trace_state.split(",")
  assert_eq(trace_state_entries.length(), 2)
  assert_true(trace_state_entries.contains("rojo=00f067aa0ba902b7"))
  assert_true(trace_state_entries.contains("congo=t61rcWkgMzE"))
}

// 测试5: Span 链接（Links）
test "Span 链接测试" {
  // 创建主 Span
  let main_span = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "b7ad6b7169203331",
    parent_span_id: None,
    name: "async operation",
    kind: SpanKind.Internal,
    start_time: 1640995200000000000L,
    end_time: Some(1640995201000000000L),
    status: Status.Ok,
    attributes: [],
    events: [],
    links: [
      {
        linked_trace_id: "e8ea7e9ac720de4a",
        linked_span_id: "028f7e1e4cde2f6a",
        attributes: [
          ("operation.type", "batch_processing"),
          ("correlation.id", "req-12345")
        ]
      },
      {
        linked_trace_id: "a3c9d5e7b8f019c2",
        linked_span_id: "1a9f8b7c6d5e4f3a",
        attributes: [
          ("operation.type", "scheduled_task"),
          ("schedule.id", "daily-cleanup")
        ]
      }
    ]
  })
  
  // 验证链接
  assert_eq(main_span.links.length(), 2)
  
  // 验证第一个链接
  let link1 = main_span.links[0]
  assert_eq(link1.linked_trace_id, "e8ea7e9ac720de4a")
  assert_eq(link1.linked_span_id, "028f7e1e4cde2f6a")
  assert_eq(link1.attributes.length(), 2)
  assert_true(link1.attributes.contains(("operation.type", "batch_processing")))
  assert_true(link1.attributes.contains(("correlation.id", "req-12345")))
  
  // 验证第二个链接
  let link2 = main_span.links[1]
  assert_eq(link2.linked_trace_id, "a3c9d5e7b8f019c2")
  assert_eq(link2.linked_span_id, "1a9f8b7c6d5e4f3a")
  assert_eq(link2.attributes.length(), 2)
  assert_true(link2.attributes.contains(("operation.type", "scheduled_task")))
  assert_true(link2.attributes.contains(("schedule.id", "daily-cleanup")))
  
  // 按操作类型过滤链接
  let batch_processing_links = main_span.links.filter(fn(l) { 
    l.attributes.contains(("operation.type", "batch_processing"))
  })
  let scheduled_task_links = main_span.links.filter(fn(l) { 
    l.attributes.contains(("operation.type", "scheduled_task"))
  })
  
  assert_eq(batch_processing_links.length(), 1)
  assert_eq(scheduled_task_links.length(), 1)
}

// 测试6: Span 状态和错误处理
test "Span 状态和错误处理测试" {
  // 创建成功的 Span
  let success_span = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "b7ad6b7169203331",
    parent_span_id: None,
    name: "successful operation",
    kind: SpanKind.Server,
    start_time: 1640995200000000000L,
    end_time: Some(1640995200500000000L),
    status: Status.Ok,
    attributes: [
      ("http.status_code", "200")
    ],
    events: [
      {
        name: "operation.completed",
        timestamp: 1640995200450000000L,
        attributes: [
          ("result", "success")
        ]
      }
    ],
    links: []
  })
  
  // 创建错误的 Span
  let error_span = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    parent_span_id: None,
    name: "failed operation",
    kind: SpanKind.Server,
    start_time: 1640995201000000000L,
    end_time: Some(1640995201500000000L),
    status: Status.Error,
    attributes: [
      ("http.status_code", "500"),
      ("error.type", "InternalServerException")
    ],
    events: [
      {
        name: "exception",
        timestamp: 1640995201300000000L,
        attributes: [
          ("exception.type", "DatabaseException"),
          ("exception.message", "Connection timeout"),
          ("exception.stacktrace", "at com.example.Service.process(Service.java:42)")
        ]
      },
      {
        name: "operation.failed",
        timestamp: 1640995201450000000L,
        attributes: [
          ("error.code", "DB_TIMEOUT")
        ]
      }
    ],
    links: []
  })
  
  // 验证成功 Span
  assert_eq(success_span.status, Status.Ok)
  assert_eq(success_span.events.length(), 1)
  assert_eq(success_span.events[0].name, "operation.completed")
  assert_true(success_span.attributes.contains(("http.status_code", "200")))
  
  // 验证错误 Span
  assert_eq(error_span.status, Status.Error)
  assert_eq(error_span.events.length(), 2)
  assert_eq(error_span.events[0].name, "exception")
  assert_eq(error_span.events[1].name, "operation.failed")
  assert_true(error_span.attributes.contains(("http.status_code", "500")))
  assert_true(error_span.attributes.contains(("error.type", "InternalServerException")))
  
  // 验证异常事件属性
  let exception_event = error_span.events[0]
  assert_true(exception_event.attributes.contains(("exception.type", "DatabaseException")))
  assert_true(exception_event.attributes.contains(("exception.message", "Connection timeout")))
  
  // 验证操作失败事件属性
  let failed_event = error_span.events[1]
  assert_true(failed_event.attributes.contains(("error.code", "DB_TIMEOUT")))
}