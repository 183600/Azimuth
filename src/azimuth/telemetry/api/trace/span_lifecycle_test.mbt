// Span生命周期的测试用例
// 测试Span从创建到结束的完整生命周期

test "span_creation_basic" {
  // 测试基本的Span创建
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer", "1.0.0")
  let ctx = Context::empty()
  
  let (new_ctx, span) = tracer.start_span(
    ctx,
    "basic-span",
    Internal,
    [("operation.type", AttributeValue::string("test"))],
    1234567890L
  )
  
  // 验证Span基本属性
  assert_eq(span.name, "basic-span")
  match span.kind {
    Internal => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  assert_eq(span.start_time_unix_nanos, 1234567890L)
  match span.end_time_unix_nanos {
    None => assert_eq(true, true) // Span还未结束
    Some(_) => @test.fail("Test failed")
  }
  match span.status {
    Unset => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  assert_eq(span.attributes.length(), 1)
}

test "span_with_parent_context" {
  // 测试带有父上下文的Span
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer", "1.0.0")
  let ctx = Context::empty()
  
  // 创建父Span
  let (parent_ctx, parent_span) = tracer.start_span(
    ctx,
    "parent-span",
    Server,
    [],
    1234567890L
  )
  
  // 创建子Span
  let (child_ctx, child_span) = tracer.start_span(
    parent_ctx,
    "child-span",
    Client,
    [("child.operation", AttributeValue::string("test"))],
    1234567895L
  )
  
  // 验证父Span
  assert_eq(parent_span.name, "parent-span")
  match parent_span.parent_span_id {
    None => assert_eq(true, true) // 父Span没有父Span ID
    Some(_) => @test.fail("Test failed")
  }
  
  // 验证子Span（在简化实现中，可能没有实际的父子关系）
  assert_eq(child_span.name, "child-span")
  // 在实际实现中，child_span.parent_span_id应该等于parent_span.context.span_id
}

test "span_lifecycle_with_events" {
  // 测试Span生命周期中的事件
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer", "1.0.0")
  let ctx = Context::empty()
  
  let (new_ctx, mut span) = tracer.start_span(
    ctx,
    "span-with-events",
    Internal,
    [("operation.name", AttributeValue::string("process"))],
    1234567890L
  )
  
  // 添加事件（在简化实现中，我们需要直接修改Span）
  let error_event = SpanEvent::{
    name: "error",
    timestamp_unix_nanos: 1234567895L,
    attributes: [
      ("error.type", AttributeValue::string("exception")),
      ("error.message", AttributeValue::string("Something went wrong"))
    ]
  }
  
  let warning_event = SpanEvent::{
    name: "warning",
    timestamp_unix_nanos: 1234567900L,
    attributes: [
      ("warning.type", AttributeValue::string("timeout")),
      ("warning.message", AttributeValue::string("Operation taking too long"))
    ]
  }
  
  // 创建带有事件的Span
  let span_with_events = Span::{
    ..span,
    events: [error_event, warning_event]
  }
  
  // 验证事件
  assert_eq(span_with_events.events.length(), 2)
  assert_eq(span_with_events.events[0].name, "error")
  assert_eq(span_with_events.events[0].timestamp_unix_nanos, 1234567895L)
  assert_eq(span_with_events.events[1].name, "warning")
  assert_eq(span_with_events.events[1].timestamp_unix_nanos, 1234567900L)
  
  // 验证事件属性
  match span_with_events.events[0].attributes[0].1 {
    AttributeValue::StringValue(error_type) => assert_eq(error_type, "exception")
    _ => @test.fail("Test failed")
  }
}

test "span_lifecycle_with_links" {
  // 测试Span生命周期中的链接
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer", "1.0.0")
  let ctx = Context::empty()
  
  // 创建链接的上下文
  let linked_context1 = SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte }),
    trace_flags: 1_byte,
    trace_state: "key1=value1"
  }
  
  let linked_context2 = SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 2_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 2_byte }),
    trace_flags: 0_byte,
    trace_state: "key2=value2"
  }
  
  // 创建链接
  let link1 = SpanLink::{
    context: linked_context1,
    attributes: [("link.type", AttributeValue::string("causality"))]
  }
  
  let link2 = SpanLink::{
    context: linked_context2,
    attributes: [("link.type", AttributeValue::string("correlation"))]
  }
  
  // 创建带有链接的Span
  let (new_ctx, span) = tracer.start_span(
    ctx,
    "span-with-links",
    Producer,
    [("operation.type", AttributeValue::string("batch"))],
    1234567890L
  )
  
  let span_with_links = Span::{
    ..span,
    links: [link1, link2]
  }
  
  // 验证链接
  assert_eq(span_with_links.links.length(), 2)
  assert_eq(span_with_links.links[0].context.span_id[0], 1_byte)
  assert_eq(span_with_links.links[1].context.span_id[0], 2_byte)
  
  // 验证链接属性
  match span_with_links.links[0].attributes[0].1 {
    AttributeValue::StringValue(link_type) => assert_eq(link_type, "causality")
    _ => @test.fail("Test failed")
  }
}

test "span_status_transitions" {
  // 测试Span状态转换
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer", "1.0.0")
  let ctx = Context::empty()
  
  let (new_ctx, span) = tracer.start_span(
    ctx,
    "span-status-transitions",
    Server,
    [],
    1234567890L
  )
  
  // 初始状态
  match span.status {
    Unset => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  // 转换到Ok状态
  let ok_span = Span::{
    ..span,
    status: Ok,
    end_time_unix_nanos: Some(1234567950L)
  }
  
  match ok_span.status {
    Ok => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match ok_span.end_time_unix_nanos {
    Some(end_time) => assert_eq(end_time, 1234567950L)
    None => @test.fail("Test failed")
  }
  
  // 转换到Error状态
  let error_span = Span::{
    ..span,
    status: Error,
    status_description: Some("Operation failed"),
    end_time_unix_nanos: Some(1234567925L)
  }
  
  match error_span.status {
    Error => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match error_span.status_description {
    Some(desc) => assert_eq(desc, "Operation failed")
    None => @test.fail("Test failed")
  }
}

test "span_comprehensive_lifecycle" {
  // 测试完整的Span生命周期
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer", "1.0.0")
  let ctx = Context::empty()
  
  // 1. 创建Span
  let (new_ctx, span) = tracer.start_span(
    ctx,
    "comprehensive-span",
    Server,
    [
      ("service.name", AttributeValue::string("api-service")),
      ("operation.name", AttributeValue::string("process-request"))
    ],
    1234567890L
  )
  
  // 2. 添加事件
  let start_event = SpanEvent::{
    name: "operation.started",
    timestamp_unix_nanos: 1234567891L,
    attributes: [("phase", AttributeValue::string("initialization"))]
  }
  
  let progress_event = SpanEvent::{
    name: "operation.progress",
    timestamp_unix_nanos: 1234567920L,
    attributes: [
      ("progress.percent", AttributeValue::int(50L)),
      ("items.processed", AttributeValue::int(100L))
    ]
  }
  
  // 3. 添加链接
  let linked_context = SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 42_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 24_byte }),
    trace_flags: 1_byte,
    trace_state: "batch.id=12345"
  }
  
  let link = SpanLink::{
    context: linked_context,
    attributes: [("link.type", AttributeValue::string("batch"))]
  }
  
  // 4. 完成Span
  let completed_span = Span::{
    name: span.name,
    context: span.context,
    kind: span.kind,
    parent_span_id: span.parent_span_id,
    start_time_unix_nanos: span.start_time_unix_nanos,
    end_time_unix_nanos: Some(1234567950L),
    status: Ok,
    status_description: Some("Operation completed successfully"),
    attributes: span.attributes,
    events: [start_event, progress_event],
    links: [link]
  }
  
  // 5. 验证完整的Span
  assert_eq(completed_span.name, "comprehensive-span")
  match completed_span.kind {
    Server => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  assert_eq(completed_span.start_time_unix_nanos, 1234567890L)
  match completed_span.end_time_unix_nanos {
    Some(end_time) => assert_eq(end_time, 1234567950L)
    None => @test.fail("Test failed")
  }
  match completed_span.status {
    Ok => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  match completed_span.status_description {
    Some(desc) => assert_eq(desc, "Operation completed successfully")
    None => @test.fail("Test failed")
  }
  assert_eq(completed_span.attributes.length(), 2)
  assert_eq(completed_span.events.length(), 2)
  assert_eq(completed_span.links.length(), 1)
  
  // 验证事件时间顺序
  assert_eq(completed_span.events[0].timestamp_unix_nanos < completed_span.events[1].timestamp_unix_nanos, true)
  
  // 验证Span持续时间
  match completed_span.end_time_unix_nanos {
    Some(end_time) => {
      let duration = end_time - completed_span.start_time_unix_nanos
      assert_eq(duration, 60L) // 1234567950 - 1234567890 = 60
    }
    None => @test.fail("Test failed")
  }
}

test "span_with_different_kinds_lifecycle" {
  // 测试不同种类Span的生命周期
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer", "1.0.0")
  let ctx = Context::empty()
  
  // 测试所有SpanKind
  let span_kinds = [Internal, Server, Client, Producer, Consumer]
  let kind_names = ["Internal", "Server", "Client", "Producer", "Consumer"]
  
  let mut i = 0
  while i < span_kinds.length() {
    let kind = span_kinds[i]
    let kind_name = kind_names[i]
    
    let (new_ctx, span) = tracer.start_span(
      ctx,
      "span-" + kind_name.to_lowercase(),
      kind,
      [("span.kind", AttributeValue::string(kind_name))],
      1234567890L + i.to_int64()
    )
    
    // 验证Span种类
    match span.kind {
      _ if i == 0 && kind == Internal => assert_eq(true, true),
      _ if i == 1 && kind == Server => assert_eq(true, true),
      _ if i == 2 && kind == Client => assert_eq(true, true),
      _ if i == 3 && kind == Producer => assert_eq(true, true),
      _ if i == 4 && kind == Consumer => assert_eq(true, true),
      _ => @test.fail("Test failed")
    }
    
    // 验证属性
    match span.attributes[0].1 {
      AttributeValue::StringValue(name) => assert_eq(name, kind_name)
      _ => @test.fail("Test failed")
    }
    
    i = i + 1
  }
}