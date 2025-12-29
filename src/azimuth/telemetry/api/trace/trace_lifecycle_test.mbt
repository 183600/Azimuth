// API Trace模块的追踪生命周期测试

test "span_context_creation_and_validation" {
  // 测试SpanContext的创建和验证
  
  // 创建基本的SpanContext
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })  // 16字节的trace_id
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 2_byte })    // 8字节的span_id
  let trace_flags = 1_byte
  let trace_state = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  
  let span_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: trace_flags,
    trace_state: trace_state
  }
  
  // 验证trace_id
  assert_eq(span_context.trace_id.length(), 16)
  assert_eq(span_context.trace_id[0], 1_byte)
  assert_eq(span_context.trace_id[15], 1_byte)
  
  // 验证span_id
  assert_eq(span_context.span_id.length(), 8)
  assert_eq(span_context.span_id[0], 2_byte)
  assert_eq(span_context.span_id[7], 2_byte)
  
  // 验证trace_flags
  assert_eq(span_context.trace_flags, 1_byte)
  
  // 验证trace_state
  assert_eq(span_context.trace_state, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
}

test "span_kinds_and_status_codes" {
  // 测试SpanKind和StatusCode枚举
  
  // 验证所有SpanKind
  let internal_kind = trace::Internal
  let server_kind = trace::Server
  let client_kind = trace::Client
  let producer_kind = trace::Producer
  let consumer_kind = trace::Consumer
  
  // 验证所有StatusCode
  let unset_status = trace::Unset
  let ok_status = trace::Ok
  let error_status = trace::Error
  
  // 使用模式匹配验证枚举值
  let validate_span_kind = fn(kind : trace::SpanKind) -> String {
    match kind {
      Internal => "Internal"
      Server => "Server"
      Client => "Client"
      Producer => "Producer"
      Consumer => "Consumer"
    }
  }
  
  let validate_status_code = fn(status : trace::StatusCode) -> String {
    match status {
      Unset => "Unset"
      Ok => "Ok"
      Error => "Error"
    }
  }
  
  assert_eq(validate_span_kind(internal_kind), "Internal")
  assert_eq(validate_span_kind(server_kind), "Server")
  assert_eq(validate_span_kind(client_kind), "Client")
  assert_eq(validate_span_kind(producer_kind), "Producer")
  assert_eq(validate_span_kind(consumer_kind), "Consumer")
  
  assert_eq(validate_status_code(unset_status), "Unset")
  assert_eq(validate_status_code(ok_status), "Ok")
  assert_eq(validate_status_code(error_status), "Error")
}

test "span_creation_and_lifecycle" {
  // 测试Span的创建和生命周期
  
  // 创建SpanContext
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 3_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 4_byte })
  let span_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  // 创建初始Span（未结束）
  let start_time = 1640995200000000000L  // 2022-01-01 00:00:00 UTC
  let span = trace::Span::{
    name: "http.request",
    context: span_context,
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: start_time,
    end_time_unix_nanos: None,
    status: trace::Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  // 验证初始状态
  assert_eq(span.name, "http.request")
  assert_eq(span.kind, trace::Server)
  assert_eq(span.parent_span_id, None)
  assert_eq(span.start_time_unix_nanos, start_time)
  assert_eq(span.end_time_unix_nanos, None)
  assert_eq(span.status, trace::Unset)
  assert_eq(span.status_description, None)
  assert_eq(span.attributes.length(), 0)
  assert_eq(span.events.length(), 0)
  assert_eq(span.links.length(), 0)
  
  // 结束Span
  let end_time = 1640995201000000000L  // 1秒后
  let ended_span = trace::Span::{
    ..span,
    end_time_unix_nanos: Some(end_time),
    status: trace::Ok,
    status_description: Some("Request completed successfully")
  }
  
  // 验证结束状态
  match ended_span.end_time_unix_nanos {
    Some(et) => assert_eq(et, end_time)
    None => assert_eq(false, true)  // 不应该到达这里
  }
  assert_eq(ended_span.status, trace::Ok)
  match ended_span.status_description {
    Some(desc) => assert_eq(desc, "Request completed successfully")
    None => assert_eq(false, true)  // 不应该到达这里
  }
}

test "span_with_parent_relationship" {
  // 测试带有父子关系的Span
  
  // 创建父SpanContext
  let parent_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 5_byte })
  let parent_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 6_byte })
  let parent_context = trace::SpanContext::{
    trace_id: parent_trace_id,
    span_id: parent_span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 创建子SpanContext（使用相同的trace_id）
  let child_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 7_byte })
  let child_context = trace::SpanContext::{
    trace_id: parent_trace_id,  // 相同的trace_id
    span_id: child_span_id,     // 不同的span_id
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 创建子Span，引用父Span
  let child_span = trace::Span::{
    name: "database.query",
    context: child_context,
    kind: trace::Internal,
    parent_span_id: Some(parent_span_id),  // 引用父span_id
    start_time_unix_nanos: 1640995202000000000L,
    end_time_unix_nanos: Some(1640995203000000000L),
    status: trace::Ok,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  // 验证父子关系
  assert_eq(child_span.context.trace_id[0], parent_context.trace_id[0])
  assert_eq(child_span.context.trace_id.length(), parent_context.trace_id.length())
  
  match child_span.parent_span_id {
    Some(parent_id) => {
      assert_eq(parent_id.length(), 8)
      assert_eq(parent_id[0], 6_byte)  // 父span_id的第一个字节
    }
    None => assert_eq(false, true)  // 不应该到达这里
  }
}

test "span_with_attributes" {
  // 测试带有属性的Span
  
  // 创建SpanContext
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 8_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 9_byte })
  let span_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  // 创建带有属性的Span
  let span = trace::Span::{
    name: "http.client.request",
    context: span_context,
    kind: trace::Client,
    parent_span_id: None,
    start_time_unix_nanos: 1640995204000000000L,
    end_time_unix_nanos: Some(1640995205000000000L),
    status: trace::Ok,
    status_description: None,
    attributes: [
      ("http.method", common::AttributeValue::string("GET")),
      ("http.url", common::AttributeValue::string("https://api.example.com/users")),
      ("http.status_code", common::AttributeValue::int(200L)),
      ("http.success", common::AttributeValue::bool(true)),
      ("http.duration", common::AttributeValue::float(250.5))
    ],
    events: [],
    links: []
  }
  
  // 验证属性
  assert_eq(span.attributes.length(), 5)
  
  assert_eq(span.attributes[0].0, "http.method")
  match span.attributes[0].1 {
    common::StringValue(s) => assert_eq(s, "GET")
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  assert_eq(span.attributes[1].0, "http.url")
  match span.attributes[1].1 {
    common::StringValue(s) => assert_eq(s, "https://api.example.com/users")
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  assert_eq(span.attributes[2].0, "http.status_code")
  match span.attributes[2].1 {
    common::IntValue(i) => assert_eq(i, 200L)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  assert_eq(span.attributes[3].0, "http.success")
  match span.attributes[3].1 {
    common::BoolValue(b) => assert_eq(b, true)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  assert_eq(span.attributes[4].0, "http.duration")
  match span.attributes[4].1 {
    common::FloatValue(f) => assert_eq(f, 250.5)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
}

test "span_with_events" {
  // 测试带有事件的Span
  
  // 创建SpanContext
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 10_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 11_byte })
  let span_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  // 创建SpanEvent
  let event1 = trace::SpanEvent::{
    name: "cache.miss",
    timestamp_unix_nanos: 1640995204500000000L,
    attributes: [
      ("cache.key", common::AttributeValue::string("user:123")),
      ("cache.type", common::AttributeValue::string("redis"))
    ]
  }
  
  let event2 = trace::SpanEvent::{
    name: "db.query.start",
    timestamp_unix_nanos: 1640995204600000000L,
    attributes: [
      ("db.query", common::AttributeValue::string("SELECT * FROM users WHERE id = ?")),
      ("db.connection", common::AttributeValue::string("conn-456"))
    ]
  }
  
  let event3 = trace::SpanEvent::{
    name: "db.query.complete",
    timestamp_unix_nanos: 1640995204800000000L,
    attributes: [
      ("db.rows", common::AttributeValue::int(1L)),
      ("db.duration", common::AttributeValue::float(15.3))
    ]
  }
  
  // 创建带有事件的Span
  let span = trace::Span::{
    name: "user.profile.fetch",
    context: span_context,
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995204000000000L,
    end_time_unix_nanos: Some(1640995205000000000L),
    status: trace::Ok,
    status_description: None,
    attributes: [],
    events: [event1, event2, event3],
    links: []
  }
  
  // 验证事件
  assert_eq(span.events.length(), 3)
  
  // 验证第一个事件
  assert_eq(span.events[0].name, "cache.miss")
  assert_eq(span.events[0].timestamp_unix_nanos, 1640995204500000000L)
  assert_eq(span.events[0].attributes.length(), 2)
  assert_eq(span.events[0].attributes[0].0, "cache.key")
  match span.events[0].attributes[0].1 {
    common::StringValue(s) => assert_eq(s, "user:123")
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证第二个事件
  assert_eq(span.events[1].name, "db.query.start")
  assert_eq(span.events[1].timestamp_unix_nanos, 1640995204600000000L)
  assert_eq(span.events[1].attributes.length(), 2)
  
  // 验证第三个事件
  assert_eq(span.events[2].name, "db.query.complete")
  assert_eq(span.events[2].timestamp_unix_nanos, 1640995204800000000L)
  assert_eq(span.events[2].attributes.length(), 2)
}

test "span_with_links" {
  // 测试带有链接的Span
  
  // 创建当前Span的SpanContext
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 12_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 13_byte })
  let span_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  // 创建链接的SpanContext
  let linked_trace_id1 = [for i = 0; i < 16; i = i + 1].map(fn(_) { 14_byte })
  let linked_span_id1 = [for i = 0; i < 8; i = i + 1].map(fn(_) { 15_byte })
  let linked_context1 = trace::SpanContext::{
    trace_id: linked_trace_id1,
    span_id: linked_span_id1,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let linked_trace_id2 = [for i = 0; i < 16; i = i + 1].map(fn(_) { 16_byte })
  let linked_span_id2 = [for i = 0; i < 8; i = i + 1].map(fn(_) { 17_byte })
  let linked_context2 = trace::SpanContext::{
    trace_id: linked_trace_id2,
    span_id: linked_span_id2,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 创建SpanLink
  let link1 = trace::SpanLink::{
    context: linked_context1,
    attributes: [
      ("link.type", common::AttributeValue::string("caused_by")),
      ("link.reason", common::AttributeValue::string("async_callback"))
    ]
  }
  
  let link2 = trace::SpanLink::{
    context: linked_context2,
    attributes: [
      ("link.type", common::AttributeValue::string("batch_parent")),
      ("link.batch.id", common::AttributeValue::string("batch-789"))
    ]
  }
  
  // 创建带有链接的Span
  let span = trace::Span::{
    name: "async.processing",
    context: span_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995206000000000L,
    end_time_unix_nanos: Some(1640995207000000000L),
    status: trace::Ok,
    status_description: None,
    attributes: [],
    events: [],
    links: [link1, link2]
  }
  
  // 验证链接
  assert_eq(span.links.length(), 2)
  
  // 验证第一个链接
  assert_eq(span.links[0].context.trace_id[0], 14_byte)
  assert_eq(span.links[0].context.span_id[0], 15_byte)
  assert_eq(span.links[0].context.trace_flags, 1_byte)
  assert_eq(span.links[0].attributes.length(), 2)
  assert_eq(span.links[0].attributes[0].0, "link.type")
  match span.links[0].attributes[0].1 {
    common::StringValue(s) => assert_eq(s, "caused_by")
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证第二个链接
  assert_eq(span.links[1].context.trace_id[0], 16_byte)
  assert_eq(span.links[1].context.span_id[0], 17_byte)
  assert_eq(span.links[1].context.trace_flags, 1_byte)
  assert_eq(span.links[1].attributes.length(), 2)
}

test "tracer_functionality" {
  // 测试Tracer的功能
  
  let tracer = trace::NoopTracer::{}
  let ctx = context::Context::empty()
  
  // 测试基本Span创建
  let (ctx1, span1) = tracer.start_span(
    ctx,
    "basic.span",
    None,
    None,
    None
  )
  
  // 验证返回的Context和Span
  assert_eq(span1.name, "basic.span")
  assert_eq(span1.kind, trace::Internal)  // 默认值
  assert_eq(span1.attributes.length(), 0)
  assert_eq(span1.events.length(), 0)
  assert_eq(span1.links.length(), 0)
  
  // 测试带参数的Span创建
  let start_time = 1640995208000000000L
  let attributes = [
    ("test.attr", common::AttributeValue::string("test_value"))
  ]
  
  let (ctx2, span2) = tracer.start_span(
    ctx1,
    "parameterized.span",
    Some(trace::Server),
    Some(attributes),
    Some(start_time)
  )
  
  // 验证参数化Span
  assert_eq(span2.name, "parameterized.span")
  assert_eq(span2.kind, trace::Server)
  assert_eq(span2.start_time_unix_nanos, start_time)
  assert_eq(span2.attributes.length(), 1)
  assert_eq(span2.attributes[0].0, "test.attr")
  match span2.attributes[0].1 {
    common::StringValue(s) => assert_eq(s, "test_value")
    _ => assert_eq(false, true)  // 不应该到达这里
  }
}

test "tracer_provider_functionality" {
  // 测试TracerProvider的功能
  
  let provider = trace::NoopTracerProvider::{}
  
  // 创建不同配置的Tracer
  let tracer1 = provider.get_tracer("test.tracer", None)
  let tracer2 = provider.get_tracer("test.tracer", Some("1.0.0"))
  let tracer3 = provider.get_tracer("another.tracer", None)
  let tracer4 = provider.get_tracer("another.tracer", Some("2.0.0"))
  
  // 验证所有Tracer都可以创建Span
  let ctx = context::Context::empty()
  
  let (ctx1, span1) = tracer1.start_span(ctx, "span.from.tracer1", None, None, None)
  let (ctx2, span2) = tracer2.start_span(ctx1, "span.from.tracer2", None, None, None)
  let (ctx3, span3) = tracer3.start_span(ctx2, "span.from.tracer3", None, None, None)
  let (ctx4, span4) = tracer4.start_span(ctx3, "span.from.tracer4", None, None, None)
  
  // 验证所有Span都有正确的名称
  assert_eq(span1.name, "span.from.tracer1")
  assert_eq(span2.name, "span.from.tracer2")
  assert_eq(span3.name, "span.from.tracer3")
  assert_eq(span4.name, "span.from.tracer4")
}

test "span_status_transitions" {
  // 测试Span状态转换
  
  // 创建SpanContext
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 18_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 19_byte })
  let span_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  // 创建初始Span（Unset状态）
  let span = trace::Span::{
    name: "status.transition.test",
    context: span_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995209000000000L,
    end_time_unix_nanos: None,
    status: trace::Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  // 验证初始状态
  assert_eq(span.status, trace::Unset)
  assert_eq(span.status_description, None)
  
  // 转换到Ok状态
  let ok_span = trace::Span::{
    ..span,
    end_time_unix_nanos: Some(1640995210000000000L),
    status: trace::Ok,
    status_description: Some("Operation completed successfully")
  }
  
  assert_eq(ok_span.status, trace::Ok)
  match ok_span.status_description {
    Some(desc) => assert_eq(desc, "Operation completed successfully")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 转换到Error状态
  let error_span = trace::Span::{
    ..span,
    end_time_unix_nanos: Some(1640995210000000000L),
    status: trace::Error,
    status_description: Some("Operation failed due to timeout")
  }
  
  assert_eq(error_span.status, trace::Error)
  match error_span.status_description {
    Some(desc) => assert_eq(desc, "Operation failed due to timeout")
    None => assert_eq(false, true)  // 不应该到达这里
  }
}