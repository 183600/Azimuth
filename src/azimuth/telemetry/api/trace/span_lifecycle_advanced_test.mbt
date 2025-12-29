// Span 生命周期和状态管理高级测试用例
// 测试Span的创建、状态转换、事件和链接管理

test "span_lifecycle_state_management" {
  // 测试Span的基本生命周期
  
  // 创建初始上下文
  let initial_context = context::Context::empty()
  
  // 创建No-op Tracer
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test-tracer", "1.0.0")
  
  // 开始一个Span
  let (ctx, span) = tracer.start_span(
    initial_context,
    "test-operation",
    trace::Server,
    [("http.method", AttributeValue::string("GET"))],
    1640995200000000000L  // 2022-01-01 00:00:00 UTC
  )
  
  // 验证Span初始状态
  assert_eq(span.name, "test-operation")
  match span.kind {
    trace::Server => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  assert_eq(span.start_time_unix_nanos, 1640995200000000000L)
  match span.status {
    trace::Unset => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  assert_eq(span.end_time_unix_nanos, None)
  assert_eq(span.attributes.length(), 1)
  assert_eq(span.attributes[0].0, "http.method")
  match span.attributes[0].1 {
    StringValue(method) => assert_eq(method, "GET")
    _ => assert_eq(false, true)
  }
  
  // 验证SpanContext
  assert_eq(span.context.trace_id.length(), 16)
  assert_eq(span.context.span_id.length(), 8)
  assert_eq(span.context.trace_flags, 0_byte)
  assert_eq(span.context.trace_state, "")
  
  // 验证初始事件和链接为空
  assert_eq(span.events.length(), 0)
  assert_eq(span.links.length(), 0)
}

test "span_status_transitions" {
  // 测试Span状态转换
  
  let initial_context = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test-tracer")
  
  // 开始Span
  let (ctx, mut span) = tracer.start_span(initial_context, "operation-with-status")
  
  // 验证初始状态
  match span.status {
    trace::Unset => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  
  // 模拟状态转换到Ok
  span = trace::Span::{
    ..span,
    status: trace::Ok,
    status_description: Some("Operation completed successfully")
  }
  
  match span.status {
    trace::Ok => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  match span.status_description {
    Some(desc) => assert_eq(desc, "Operation completed successfully")
    None => assert_eq(false, true)
  }
  
  // 模拟状态转换到Error
  span = trace::Span::{
    ..span,
    status: trace::Error,
    status_description: Some("Operation failed due to timeout")
  }
  
  match span.status {
    trace::Error => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  match span.status_description {
    Some(desc) => assert_eq(desc, "Operation failed due to timeout")
    None => assert_eq(false, true)
  }
}

test "span_events_and_links_management" {
  // 测试Span事件和链接管理
  
  let initial_context = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test-tracer")
  
  // 开始Span
  let (ctx, span) = tracer.start_span(initial_context, "operation-with-events")
  
  // 创建Span事件
  let event1 = trace::SpanEvent::{
    name: "database.query",
    timestamp_unix_nanos: 1640995200000000000L,
    attributes: [
      ("db.statement", AttributeValue::string("SELECT * FROM users")),
      ("db.duration_ms", AttributeValue::int(150L))
    ]
  }
  
  let event2 = trace::SpanEvent::{
    name: "cache.hit",
    timestamp_unix_nanos: 1640995200100000000L,
    attributes: [
      ("cache.key", AttributeValue::string("user:123")),
      ("cache.hit", AttributeValue::bool(true))
    ]
  }
  
  // 创建Span链接
  let linked_context = trace::SpanContext::{
    trace_id: Array::make(16, 1_byte),
    span_id: Array::make(8, 2_byte),
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  let link = trace::SpanLink::{
    context: linked_context,
    attributes: [
      ("link.type", AttributeValue::string("parent")),
      ("relationship", AttributeValue::string("follows-from"))
    ]
  }
  
  // 创建包含事件和链接的Span
  let span_with_events = trace::Span::{
    ..span,
    events: [event1, event2],
    links: [link]
  }
  
  // 验证事件
  assert_eq(span_with_events.events.length(), 2)
  assert_eq(span_with_events.events[0].name, "database.query")
  assert_eq(span_with_events.events[0].timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(span_with_events.events[0].attributes.length(), 2)
  assert_eq(span_with_events.events[0].attributes[0].0, "db.statement")
  match span_with_events.events[0].attributes[0].1 {
    StringValue(stmt) => assert_eq(stmt, "SELECT * FROM users")
    _ => assert_eq(false, true)
  }
  
  assert_eq(span_with_events.events[1].name, "cache.hit")
  assert_eq(span_with_events.events[1].timestamp_unix_nanos, 1640995200100000000L)
  assert_eq(span_with_events.events[1].attributes.length(), 2)
  
  // 验证链接
  assert_eq(span_with_events.links.length(), 1)
  assert_eq(span_with_events.links[0].context.trace_id.length(), 16)
  assert_eq(span_with_events.links[0].context.span_id.length(), 8)
  assert_eq(span_with_events.links[0].context.trace_flags, 1_byte)
  assert_eq(span_with_events.links[0].context.trace_state, "key1=value1,key2=value2")
  assert_eq(span_with_events.links[0].attributes.length(), 2)
  assert_eq(span_with_events.links[0].attributes[0].0, "link.type")
  match span_with_events.links[0].attributes[0].1 {
    StringValue(link_type) => assert_eq(link_type, "parent")
    _ => assert_eq(false, true)
  }
}

test "span_hierarchy_and_parent_relationships" {
  // 测试Span层次结构和父子关系
  
  let initial_context = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test-tracer")
  
  // 创建父Span
  let (parent_ctx, parent_span) = tracer.start_span(
    initial_context,
    "parent-operation",
    trace::Server,
    [("service.name", AttributeValue::string("api-service"))]
  )
  
  // 创建子Span
  let (child_ctx, child_span) = tracer.start_span(
    parent_ctx,
    "child-operation",
    trace::Internal,
    [("operation.type", AttributeValue::string("database-query"))]
  )
  
  // 验证子Span的父关系
  assert_eq(child_span.name, "child-operation")
  match child_span.kind {
    trace::Internal => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  
  // 创建具有明确父SpanID的子Span
  let parent_span_id = Array::make(8, 42_byte)
  let child_with_explicit_parent = tracer.start_span(
    parent_ctx,
    "child-with-explicit-parent"
  )
  
  let child_span_with_parent = trace::Span::{
    ..child_with_explicit_parent.1,
    parent_span_id: Some(parent_span_id)
  }
  
  // 验证显式父SpanID
  match child_span_with_parent.parent_span_id {
    Some(parent_id) => {
      assert_eq(parent_id.length(), 8)
      assert_eq(parent_id[0], 42_byte)
    }
    None => assert_eq(false, true)
  }
  
  // 验证不同SpanKind的行为
  let server_span = tracer.start_span(initial_context, "server-span", trace::Server)
  match server_span.1.kind {
    trace::Server => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  
  let client_span = tracer.start_span(initial_context, "client-span", trace::Client)
  match client_span.1.kind {
    trace::Client => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  
  let producer_span = tracer.start_span(initial_context, "producer-span", trace::Producer)
  match producer_span.1.kind {
    trace::Producer => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  
  let consumer_span = tracer.start_span(initial_context, "consumer-span", trace::Consumer)
  match consumer_span.1.kind {
    trace::Consumer => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
}

test "span_timing_and_duration_calculations" {
  // 测试Span时间和持续时间计算
  
  let initial_context = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test-tracer")
  
  // 创建具有特定开始时间的Span
  let start_time = 1640995200000000000L  // 2022-01-01 00:00:00 UTC
  let (ctx, span) = tracer.start_span(
    initial_context,
    "timed-operation",
    trace::Internal,
    [],
    Some(start_time)
  )
  
  // 验证开始时间
  assert_eq(span.start_time_unix_nanos, start_time)
  assert_eq(span.end_time_unix_nanos, None)
  
  // 模拟Span结束
  let end_time = 1640995201000000000L  // 1秒后
  let completed_span = trace::Span::{
    ..span,
    end_time_unix_nanos: Some(end_time),
    status: trace::Ok
  }
  
  // 验证结束时间和持续时间
  match completed_span.end_time_unix_nanos {
    Some(end) => {
      assert_eq(end, end_time)
      let duration = end - start_time
      assert_eq(duration, 1000000000L)  // 1秒，以纳秒为单位
    }
    None => assert_eq(false, true)
  }
  
  match completed_span.status {
    trace::Ok => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  
  // 测试零持续时间Span
  let instant_start = 1640995202000000000L
  let (ctx2, instant_span) = tracer.start_span(
    initial_context,
    "instant-operation",
    trace::Internal,
    [],
    Some(instant_start)
  )
  
  let completed_instant_span = trace::Span::{
    ..instant_span,
    end_time_unix_nanos: Some(instant_start),  // 立即结束
    status: trace::Ok
  }
  
  match completed_instant_span.end_time_unix_nanos {
    Some(end) => {
      let duration = end - instant_start
      assert_eq(duration, 0L)  // 零持续时间
    }
    None => assert_eq(false, true)
  }
}