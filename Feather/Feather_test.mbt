// Feather_test.mbt - 测试Feather Telemetry套件

test "basic_tracer_creation" {
  // 测试基本的Tracer创建
  init_basic_tracing()
  let tracer = get_tracer("test")
  @assert.not_eq!(tracer, None)
}

test "span_creation_and_lifecycle" {
  init_basic_tracing()
  let tracer = get_tracer("test")
  let (ctx, span) = tracer.start_span("test_span")
  
  @assert.assert!(span.is_recording())
  @assert.assert!(span.span_context().trace_id.length() == 16)
  @assert.assert!(span.span_context().span_id.length() == 8)
  
  span.set_attribute("test.key", "test.value")
  span.add_event("test_event")
  span.set_status(Ok, "Test completed")
  span.end()
  
  @assert.assert!(!span.is_recording())
}

test "context_propagation" {
  init_basic_tracing()
  let tracer = get_tracer("test")
  
  // 创建父span
  let (parent_ctx, parent_span) = tracer.start_span("parent_span")
  
  // 在父span的context中创建子span
  let (child_ctx, child_span) = tracer.start_span("child_span", Some(Internal), [], [], None, parent_ctx)
  
  // 验证trace ID相同
  let parent_trace_id = parent_span.span_context().trace_id
  let child_trace_id = child_span.span_context().trace_id
  
  @assert.assert!(parent_trace_id == child_trace_id)
  
  // 验证span ID不同
  let parent_span_id = parent_span.span_context().span_id
  let child_span_id = child_span.span_context().span_id
  
  @assert.assert!(parent_span_id != child_span_id)
  
  child_span.end()
  parent_span.end()
}

test "w3c_trace_context_propagation" {
  let propagator = W3CTraceContextPropagator::new()
  let tracer = get_tracer("test")
  
  // 创建span和context
  let (ctx, span) = tracer.start_span("test_span")
  
  // 创建carrier并注入context
  let carrier = HttpHeadersCarrier::new(Map::new())
  propagator.inject(ctx, carrier)
  
  // 验证traceparent header存在
  match carrier.get("traceparent") {
    Some(trace_parent) => {
      @assert.assert!(trace_parent.length() > 0)
      @assert.assert!(trace_parent.contains("-"))
    }
    None => @assert.assert!(false, "traceparent header not found")
  }
  
  span.end()
}

test "span_attributes_and_events" {
  init_basic_tracing()
  let tracer = get_tracer("test")
  let (ctx, span) = tracer.start_span("test_span")
  
  // 测试各种属性类型
  span.set_attribute("string.attr", "string_value")
  span.set_attribute("int.attr", 42L)
  span.set_attribute("float.attr", 3.14)
  span.set_attribute("bool.attr", true)
  
  // 测试事件
  span.add_event("event1", [("key1", "value1")])
  span.add_event("event2", [("key2", 42L)], 1234567890L)
  
  // 测试异常记录
  span.record_exception("Test exception", [("exception.type", "test")])
  
  span.end()
}

test "trace_span_convenience_function" {
  init_basic_tracing()
  
  let result = trace_span("convenience_test", fn(ctx, span) {
    span.set_attribute("test", "convenience")
    span.add_event("in_convenience_function")
    "success"
  })
  
  @assert.assert!(result == "success")
}

test "multiple_spans_with_relationships" {
  init_basic_tracing()
  let tracer = get_tracer("test")
  
  // 创建根span
  let (root_ctx, root_span) = tracer.start_span("root_operation", Some(Server))
  root_span.set_attribute("service.name", "test-service")
  
  // 创建子操作
  let (op1_ctx, op1_span) = tracer.start_span("operation_1", Some(Internal), [], [], None, root_ctx)
  op1_span.set_attribute("operation.type", "database")
  op1_span.add_event("db.query.start")
  op1_span.set_status(Ok, "Query completed")
  op1_span.end()
  
  // 创建另一个子操作
  let (op2_ctx, op2_span) = tracer.start_span("operation_2", Some(Client), [], [], None, root_ctx)
  op2_span.set_attribute("operation.type", "http")
  op2_span.add_event("http.request.start")
  op2_span.set_status(Ok, "Request completed")
  op2_span.end()
  
  root_span.set_status(Ok, "All operations completed")
  root_span.end()
}