// Azimuth 分布式追踪跨服务传播测试用例
// 专注于测试分布式系统中追踪上下文的跨服务传播和一致性

// 测试1: 跨服务追踪上下文创建和传播
test "跨服务追踪上下文创建和传播" {
  // 创建根追踪上下文
  let root_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let root_span_id = "b7ad6b7169203331"
  
  let root_span_context = {
    trace_id: root_trace_id,
    span_id: root_span_id,
    sampled: true,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 验证根上下文
  assert_eq(root_span_context.trace_id, root_trace_id)
  assert_eq(root_span_context.span_id, root_span_id)
  assert_true(root_span_context.sampled)
  assert_eq(root_span_context.trace_state, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  
  // 服务A创建子Span
  let service_a_span_id = "00f067aa0ba902b7"
  let service_a_context = {
    trace_id: root_trace_id,
    span_id: service_a_span_id,
    sampled: true,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 验证追踪ID一致性
  assert_eq(service_a_context.trace_id, root_span_context.trace_id)
  assert_eq(service_a_context.sampled, root_span_context.sampled)
  assert_eq(service_a_context.trace_state, root_span_context.trace_state)
  
  // 服务B创建子Span
  let service_b_span_id = "t61rcWkgMzE"
  let service_b_context = {
    trace_id: root_trace_id,
    span_id: service_b_span_id,
    sampled: true,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 验证跨服务追踪一致性
  assert_eq(service_b_context.trace_id, root_span_context.trace_id)
  assert_eq(service_b_context.trace_id, service_a_context.trace_id)
}

// 测试2: HTTP头部传播机制
test "HTTP头部传播机制" {
  // 创建传播用的头部载体
  let carrier = {
    headers: [
      ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
      ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"),
      ("x-request-id", "req-123456"),
      ("x-b3-traceid", "0af7651916cd43dd8448eb211c80319c"),
      ("x-b3-spanid", "b7ad6b7169203331"),
      ("x-b3-sampled", "1")
    ]
  }
  
  // 验证traceparent头部格式
  let traceparent_header = carrier.headers.find(fn(header) { header.0 == "traceparent" })
  match traceparent_header {
    Some(header) => {
      let parts = header.1.split("-")
      assert_eq(parts.length(), 4)
      assert_eq(parts[0], "00")  // version
      assert_eq(parts[1], "0af7651916cd43dd8448eb211c80319c")  // trace-id
      assert_eq(parts[2], "b7ad6b7169203331")  // parent-span-id
      assert_eq(parts[3], "01")  // trace-flags
    }
    None => assert_true(false)
  }
  
  // 验证tracestate头部
  let tracestate_header = carrier.headers.find(fn(header) { header.0 == "tracestate" })
  match tracestate_header {
    Some(header) => {
      assert_eq(header.1, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
    }
    None => assert_true(false)
  }
  
  // 验证B3格式头部
  let b3_traceid = carrier.headers.find(fn(header) { header.0 == "x-b3-traceid" })
  let b3_spanid = carrier.headers.find(fn(header) { header.0 == "x-b3-spanid" })
  let b3_sampled = carrier.headers.find(fn(header) { header.0 == "x-b3-sampled" })
  
  match b3_traceid {
    Some(header) => assert_eq(header.1, "0af7651916cd43dd8448eb211c80319c")
    None => assert_true(false)
  }
  
  match b3_spanid {
    Some(header) => assert_eq(header.1, "b7ad6b7169203331")
    None => assert_true(false)
  }
  
  match b3_sampled {
    Some(header) => assert_eq(header.1, "1")
    None => assert_true(false)
  }
}

// 测试3: 跨服务Baggage传播
test "跨服务Baggage传播" {
  // 创建初始Baggage
  let initial_baggage = {
    entries: [
      ("user.id", "user123"),
      ("request.origin", "mobile"),
      ("tenant.id", "tenant456"),
      ("session.id", "sess789")
    ]
  }
  
  // 验证初始Baggage
  assert_eq(initial_baggage.entries.length(), 4)
  assert_true(initial_baggage.entries.contains(fn(entry) { entry.0 == "user.id" && entry.1 == "user123" }))
  assert_true(initial_baggage.entries.contains(fn(entry) { entry.0 == "request.origin" && entry.1 == "mobile" }))
  
  // 服务A添加新的Baggage项
  let service_a_baggage = {
    entries: initial_baggage.entries + [
      ("service.a.timestamp", "1640995200"),
      ("service.a.version", "1.2.3")
    ]
  }
  
  // 验证Baggage传播和扩展
  assert_eq(service_a_baggage.entries.length(), 6)
  assert_true(service_a_baggage.entries.contains(fn(entry) { entry.0 == "service.a.timestamp" }))
  
  // 服务B修改现有Baggage项
  let service_b_baggage = {
    entries: service_a_baggage.entries.map(fn(entry) {
      if entry.0 == "request.origin" {
        ("request.origin", "mobile-updated")
      } else {
        entry
      }
    }) + [
      ("service.b.latency", "150ms")
    ]
  }
  
  // 验证Baggage修改
  assert_eq(service_b_baggage.entries.length(), 7)
  assert_true(service_b_baggage.entries.contains(fn(entry) { 
    entry.0 == "request.origin" && entry.1 == "mobile-updated" 
  }))
  assert_true(service_b_baggage.entries.contains(fn(entry) { 
    entry.0 == "service.b.latency" && entry.1 == "150ms" 
  }))
}

// 测试4: 分布式追踪链路完整性
test "分布式追踪链路完整性" {
  // 模拟分布式服务调用链
  let service_call_chain = [
    {service: "gateway", span_id: "span001", parent_span_id: "root", duration_ms: 50},
    {service: "auth", span_id: "span002", parent_span_id: "span001", duration_ms: 120},
    {service: "user-service", span_id: "span003", parent_span_id: "span002", duration_ms: 80},
    {service: "database", span_id: "span004", parent_span_id: "span003", duration_ms: 200},
    {service: "cache", span_id: "span005", parent_span_id: "span003", duration_ms: 30},
    {service: "notification", span_id: "span006", parent_span_id: "span001", duration_ms: 60}
  ]
  
  // 验证调用链完整性
  assert_eq(service_call_chain.length(), 6)
  
  // 验证父子关系
  let auth_span = service_call_chain.find(fn(span) { span.service == "auth" })
  match auth_span {
    Some(span) => assert_eq(span.parent_span_id, "span001")
    None => assert_true(false)
  }
  
  let user_service_span = service_call_chain.find(fn(span) { span.service == "user-service" })
  match user_service_span {
    Some(span) => assert_eq(span.parent_span_id, "span002")
    None => assert_true(false)
  }
  
  // 验证并行调用（cache和database都从user-service调用）
  let db_span = service_call_chain.find(fn(span) { span.service == "database" })
  let cache_span = service_call_chain.find(fn(span) { span.service == "cache" })
  
  match db_span {
    Some(span) => assert_eq(span.parent_span_id, "span003")
    None => assert_true(false)
  }
  
  match cache_span {
    Some(span) => assert_eq(span.parent_span_id, "span003")
    None => assert_true(false)
  }
  
  // 计算总持续时间
  let total_duration = service_call_chain.fold(0, fn(acc, span) { acc + span.duration_ms })
  assert_eq(total_duration, 540)
  
  // 验证最慢的服务
  let slowest_service = service_call_chain.fold(
    {service: "", duration_ms: 0}, 
    fn(acc, span) { 
      if span.duration_ms > acc.duration_ms { span } else { acc } 
    }
  )
  assert_eq(slowest_service.service, "database")
  assert_eq(slowest_service.duration_ms, 200)
}

// 测试5: 跨服务追踪上下文注入和提取
test "跨服务追踪上下文注入和提取" {
  // 创建原始Span上下文
  let original_context = {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    sampled: true,
    trace_state: "vendor1=value1,vendor2=value2"
  }
  
  // 模拟注入操作
  let injected_headers = [
    ("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"),
    ("tracestate", "vendor1=value1,vendor2=value2"),
    ("x-correlation-id", "corr-12345")
  ]
  
  // 模拟提取操作
  let extracted_traceparent = injected_headers.find(fn(header) { header.0 == "traceparent" })
  let extracted_tracestate = injected_headers.find(fn(header) { header.0 == "tracestate" })
  
  // 验证提取的上下文
  match extracted_traceparent {
    Some(header) => {
      let parts = header.1.split("-")
      assert_eq(parts[1], original_context.trace_id)
      assert_eq(parts[2], original_context.span_id)
    }
    None => assert_true(false)
  }
  
  match extracted_tracestate {
    Some(header) => assert_eq(header.1, original_context.trace_state)
    None => assert_true(false)
  }
  
  // 验证重建的上下文
  let reconstructed_context = {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    sampled: true,
    trace_state: "vendor1=value1,vendor2=value2"
  }
  
  assert_eq(reconstructed_context.trace_id, original_context.trace_id)
  assert_eq(reconstructed_context.span_id, original_context.span_id)
  assert_eq(reconstructed_context.sampled, original_context.sampled)
  assert_eq(reconstructed_context.trace_state, original_context.trace_state)
}

// 测试6: 异常情况下的追踪传播
test "异常情况下的追踪传播" {
  // 模拟异常服务调用
  let error_call_chain = [
    {service: "gateway", span_id: "span001", parent_span_id: "root", status: "ok", error: None},
    {service: "auth", span_id: "span002", parent_span_id: "span001", status: "error", error: Some("authentication failed")},
    {service: "user-service", span_id: "span003", parent_span_id: "span002", status: "ok", error: None},
    {service: "database", span_id: "span004", parent_span_id: "span003", status: "timeout", error: Some("connection timeout")}
  ]
  
  // 验证错误传播
  let auth_span = error_call_chain.find(fn(span) { span.service == "auth" })
  match auth_span {
    Some(span) => {
      assert_eq(span.status, "error")
      match span.error {
        Some(error_msg) => assert_eq(error_msg, "authentication failed")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证在父服务失败的情况下子服务仍然保持追踪上下文
  let user_service_span = error_call_chain.find(fn(span) { span.service == "user-service" })
  match user_service_span {
    Some(span) => {
      assert_eq(span.parent_span_id, "span002")  // 仍然引用失败的auth服务
      assert_eq(span.status, "ok")  // 但自身状态正常
    }
    None => assert_true(false)
  }
  
  // 统计错误数量
  let error_count = error_call_chain.fold(0, fn(acc, span) { 
    if span.status != "ok" { acc + 1 } else { acc } 
  })
  assert_eq(error_count, 2)
  
  // 验证追踪链路完整性即使在错误情况下也得以保持
  let all_spans_have_parent = error_call_chain.all(fn(span) { 
    span.parent_span_id != "" || span.service == "gateway"
  })
  assert_true(all_spans_have_parent)
}