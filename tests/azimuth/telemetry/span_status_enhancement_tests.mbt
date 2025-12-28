// Azimuth Telemetry System - Span Status and Event Management Tests
// 测试Span状态和事件管理功能

test "Span状态码转换和验证测试" {
  // 测试所有状态码
  let unset_status = Unset
  let ok_status = Ok
  let error_status = Error
  
  // 验证状态码比较
  assert_eq(unset_status, Unset)
  assert_eq(ok_status, Ok)
  assert_eq(error_status, Error)
  
  assert_true(unset_status != Ok)
  assert_true(ok_status != Error)
  assert_true(error_status != Unset)
  
  // 测试Span状态管理
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "")
  let span = Span::new("test-span", Server, span_ctx)
  
  // 初始状态应该是Unset
  assert_eq(Span::status(span), Unset)
  
  // 测试状态设置
  Span::set_status(span, Ok, Some("Operation completed"))
  Span::set_status(span, Error, Some("Operation failed"))
  Span::set_status(span, Unset, None)
  
  // 测试不同SpanKind的状态管理
  let internal_span = Span::new("internal", Internal, span_ctx)
  let client_span = Span::new("client", Client, span_ctx)
  let server_span = Span::new("server", Server, span_ctx)
  let producer_span = Span::new("producer", Producer, span_ctx)
  let consumer_span = Span::new("consumer", Consumer, span_ctx)
  
  assert_eq(Span::kind(internal_span), Internal)
  assert_eq(Span::kind(client_span), Client)
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::kind(producer_span), Producer)
  assert_eq(Span::kind(consumer_span), Consumer)
  
  // 为不同类型的Span设置状态
  Span::set_status(internal_span, Ok, None)
  Span::set_status(client_span, Error, Some("Client error"))
  Span::set_status(server_span, Ok, Some("Request processed"))
  Span::set_status(producer_span, Unset, None)
  Span::set_status(consumer_span, Error, Some("Consumer failed"))
}

test "Span事件记录和属性管理测试" {
  // 创建测试Span
  let span_ctx = SpanContext::new("trace-event", "span-event", true, "")
  let span = Span::new("event-test-span", Internal, span_ctx)
  
  // 测试基本事件记录
  Span::add_event(span, "start-event", None)
  Span::add_event(span, "middle-event", None)
  Span::add_event(span, "end-event", None)
  
  // 测试带属性的事件记录
  let event1_attrs = [("event.type", StringValue("user.action")), ("user.id", IntValue(12345))]
  let event2_attrs = [("http.method", StringValue("GET")), ("http.status", IntValue(200))]
  let event3_attrs = [("db.query", StringValue("SELECT * FROM users")), ("db.duration", FloatValue(150.5))]
  
  Span::add_event(span, "user-login", Some(event1_attrs))
  Span::add_event(span, "http-request", Some(event2_attrs))
  Span::add_event(span, "database-query", Some(event3_attrs))
  
  // 测试复杂事件属性
  let complex_attrs = [
    ("service.name", StringValue("auth-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("instance-abc123")),
    ("request.id", StringValue("req-def456")),
    ("response.time", FloatValue(85.3)),
    ("success", BoolValue(true)),
    ("error.count", IntValue(0))
  ]
  
  Span::add_event(span, "service-call", Some(complex_attrs))
  
  // 测试边界情况的事件
  Span::add_event(span, "", None) // 空事件名
  Span::add_event(span, "event-with-empty-attrs", Some([])) // 空属性数组
  
  // 测试特殊字符的事件名
  Span::add_event(span, "event/with/slashes", None)
  Span::add_event(span, "event-with-dashes", None)
  Span::add_event(span, "event_with_underscores", None)
  Span::add_event(span, "event.with.dots", None)
  
  // 测试长事件名
  let long_event_name = "this-is-a-very-long-event-name-that-might-be-used-in-some-special-scenarios-for-testing-purposes"
  Span::add_event(span, long_event_name, None)
}

test "Span生命周期状态管理测试" {
  // 测试Span的完整生命周期
  let span_ctx = SpanContext::new("trace-lifecycle", "span-lifecycle", true, "")
  let span = Span::new("lifecycle-test", Server, span_ctx)
  
  // 初始状态验证
  assert_eq(Span::name(span), "lifecycle-test")
  assert_eq(Span::kind(span), Server)
  assert_true(Span::is_recording(span))
  assert_eq(Span::span_context(span), span_ctx)
  
  // 在记录状态下的操作
  Span::add_event(span, "lifecycle-start", None)
  Span::set_status(span, Ok, Some("Processing started"))
  
  // 验证SpanContext信息
  let ctx = Span::span_context(span)
  assert_eq(SpanContext::trace_id(ctx), "trace-lifecycle")
  assert_eq(SpanContext::span_id(ctx), "span-lifecycle")
  assert_true(SpanContext::is_sampled(ctx))
  assert_true(SpanContext::is_valid(ctx))
  
  // 测试不同阶段的Span操作
  Span::add_event(span, "middleware-enter", Some([("middleware.name", StringValue("auth"))]))
  Span::set_status(span, Unset, None)
  Span::add_event(span, "middleware-exit", Some([("middleware.name", StringValue("auth")), ("duration", IntValue(15))]))
  
  Span::add_event(span, "business-logic-start", None)
  Span::set_status(span, Ok, Some("Business logic executing"))
  Span::add_event(span, "business-logic-end", Some([("records.processed", IntValue(100))]))
  
  // 结束Span
  Span::end(span)
  
  // 结束后的操作（在简化实现中可能仍然有效）
  Span::add_event(span, "post-end-event", None)
  Span::set_status(span, Error, Some("Post-end error"))
}

test "多Span依赖和层次关系测试" {
  // 创建父Span
  let parent_ctx = SpanContext::new("trace-hierarchy", "parent-span", true, "")
  let parent_span = Span::new("parent-operation", Server, parent_ctx)
  
  // 创建子Span
  let child1_ctx = SpanContext::new("trace-hierarchy", "child-span-1", true, "")
  let child1_span = Span::new("child-operation-1", Client, child1_ctx)
  
  let child2_ctx = SpanContext::new("trace-hierarchy", "child-span-2", true, "")
  let child2_span = Span::new("child-operation-2", Internal, child2_ctx)
  
  // 创建孙级Span
  let grandchild_ctx = SpanContext::new("trace-hierarchy", "grandchild-span", true, "")
  let grandchild_span = Span::new("grandchild-operation", Producer, grandchild_ctx)
  
  // 设置父Span的事件和状态
  Span::add_event(parent_span, "parent-start", None)
  Span::set_status(parent_span, Ok, Some("Parent operation started"))
  
  // 设置子Span的事件和状态
  Span::add_event(child1_span, "child1-start", Some([("parent", StringValue("parent-operation"))]))
  Span::set_status(child1_span, Ok, Some("Child 1 completed"))
  
  Span::add_event(child2_span, "child2-start", Some([("parent", StringValue("parent-operation"))]))
  Span::set_status(child2_span, Error, Some("Child 2 failed"))
  
  // 设置孙级Span的事件和状态
  Span::add_event(grandchild_span, "grandchild-start", Some([
    ("parent", StringValue("child-operation-1")),
    ("grandparent", StringValue("parent-operation"))
  ]))
  Span::set_status(grandchild_span, Ok, Some("Grandchild completed"))
  
  // 验证所有Span的基本属性
  assert_eq(Span::name(parent_span), "parent-operation")
  assert_eq(Span::name(child1_span), "child-operation-1")
  assert_eq(Span::name(child2_span), "child-operation-2")
  assert_eq(Span::name(grandchild_span), "grandchild-operation")
  
  assert_eq(Span::kind(parent_span), Server)
  assert_eq(Span::kind(child1_span), Client)
  assert_eq(Span::kind(child2_span), Internal)
  assert_eq(Span::kind(grandchild_span), Producer)
  
  // 按顺序结束Span（模拟实际执行顺序）
  Span::end(grandchild_span)
  Span::end(child1_span)
  Span::end(child2_span)
  Span::end(parent_span)
}

test "Span并发和线程安全测试" {
  // 创建多个Span模拟并发场景
  let spans = []
  let span_contexts = []
  
  // 创建多个Span
  for i in 0..<5 {
    let trace_id = "concurrent-trace-" + i.to_string()
    let span_id = "concurrent-span-" + i.to_string()
    let ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("concurrent-operation-" + i.to_string(), Internal, ctx)
    spans.push(span)
    span_contexts.push(ctx)
  }
  
  // 为每个Span添加事件和设置状态
  for i in 0..<spans.length() {
    let span = spans[i]
    Span::add_event(span, "concurrent-start-" + i.to_string(), Some([
      ("worker.id", IntValue(i)),
      ("thread.name", StringValue("worker-" + i.to_string()))
    ]))
    
    if i % 2 == 0 {
      Span::set_status(span, Ok, Some("Worker " + i.to_string() + " succeeded"))
    } else {
      Span::set_status(span, Error, Some("Worker " + i.to_string() + " failed"))
    }
    
    Span::add_event(span, "concurrent-end-" + i.to_string(), Some([
      ("worker.id", IntValue(i)),
      ("completed", BoolValue(true))
    ]))
  }
  
  // 验证所有Span的属性
  for i in 0..<spans.length() {
    let span = spans[i]
    let expected_name = "concurrent-operation-" + i.to_string()
    assert_eq(Span::name(span), expected_name)
    assert_eq(Span::kind(span), Internal)
    assert_true(Span::is_recording(span))
    
    let ctx = Span::span_context(span)
    let expected_trace_id = "concurrent-trace-" + i.to_string()
    let expected_span_id = "concurrent-span-" + i.to_string()
    assert_eq(SpanContext::trace_id(ctx), expected_trace_id)
    assert_eq(SpanContext::span_id(ctx), expected_span_id)
  }
  
  // 结束所有Span
  for i in 0..<spans.length() {
    Span::end(spans[i])
  }
}