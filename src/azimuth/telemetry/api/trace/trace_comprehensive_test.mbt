// Trace模块的综合测试用例
test "span_context_creation_and_properties" {
  // 测试SpanContext的创建和属性
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte })
  let trace_flags = 1_byte
  let trace_state = "rojo=00f067aa0ba902b7"
  
  let span_context = SpanContext::{
    trace_id,
    span_id,
    trace_flags,
    trace_state
  }
  
  // 验证trace_id长度
  assert_eq(span_context.trace_id.length(), 16)
  
  // 验证span_id长度
  assert_eq(span_context.span_id.length(), 8)
  
  // 验证trace_flags
  assert_eq(span_context.trace_flags, 1_byte)
  
  // 验证trace_state
  assert_eq(span_context.trace_state, "rojo=00f067aa0ba902b7")
}

test "span_creation_with_all_fields" {
  // 测试创建包含所有字段的Span
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte })
  let parent_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 2_byte })
  
  let span_context = SpanContext::{
    trace_id,
    span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let attributes = [
    ("http.method", AttributeValue::string("GET")),
    ("http.url", AttributeValue::string("https://example.com/api")),
    ("http.status_code", AttributeValue::int(200L))
  ]
  
  let events = [
    SpanEvent::{
      name: "database.query",
      timestamp_unix_nanos: 1640995200000000000L,
      attributes: [
        ("db.statement", AttributeValue::string("SELECT * FROM users"))
      ]
    }
  ]
  
  let links = [
    SpanLink::{
      context: span_context,
      attributes: [
        ("link.type", AttributeValue::string("parent"))
      ]
    }
  ]
  
  let span = Span::{
    name: "http.request",
    context: span_context,
    kind: Server,
    parent_span_id: Some(parent_span_id),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200100000000L),
    status: Ok,
    status_description: Some("Request completed successfully"),
    attributes,
    events,
    links
  }
  
  // 验证基本属性
  assert_eq(span.name, "http.request")
  assert_eq(span.kind, Server)
  assert_eq(span.status, Ok)
  
  // 验证时间
  assert_eq(span.start_time_unix_nanos, 1640995200000000000L)
  match span.end_time_unix_nanos {
    Some(end_time) => assert_eq(end_time, 1640995200100000000L)
    None => @test.fail("Test failed")
  }
  
  // 验证属性
  assert_eq(span.attributes.length(), 3)
  assert_eq(span.attributes[0].0, "http.method")
  
  // 验证事件
  assert_eq(span.events.length(), 1)
  assert_eq(span.events[0].name, "database.query")
  
  // 验证链接
  assert_eq(span.links.length(), 1)
  assert_eq(span.links[0].attributes.length(), 1)
}

test "span_kinds_enumeration" {
  // 测试所有SpanKind枚举值
  let internal_span = NoopTracer::start_span(
    Context::empty(), 
    "internal-operation", 
    kind?: Internal
  ).1
  
  let server_span = NoopTracer::start_span(
    Context::empty(), 
    "server-request", 
    kind?: Server
  ).1
  
  let client_span = NoopTracer::start_span(
    Context::empty(), 
    "client-request", 
    kind?: Client
  ).1
  
  let producer_span = NoopTracer::start_span(
    Context::empty(), 
    "message-produce", 
    kind?: Producer
  ).1
  
  let consumer_span = NoopTracer::start_span(
    Context::empty(), 
    "message-consume", 
    kind?: Consumer
  ).1
  
  // 验证所有SpanKind
  assert_eq(internal_span.kind, Internal)
  assert_eq(server_span.kind, Server)
  assert_eq(client_span.kind, Client)
  assert_eq(producer_span.kind, Producer)
  assert_eq(consumer_span.kind, Consumer)
}

test "span_status_codes" {
  // 测试所有StatusCode枚举值
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte })
  let span_context = SpanContext::{
    trace_id,
    span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  let unset_span = Span::{
    name: "unset-span",
    context: span_context,
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 0L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  let ok_span = Span::{
    name: "ok-span",
    context: span_context,
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 0L,
    end_time_unix_nanos: None,
    status: Ok,
    status_description: Some("Operation succeeded"),
    attributes: [],
    events: [],
    links: []
  }
  
  let error_span = Span::{
    name: "error-span",
    context: span_context,
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 0L,
    end_time_unix_nanos: None,
    status: Error,
    status_description: Some("Operation failed"),
    attributes: [],
    events: [],
    links: []
  }
  
  // 验证所有StatusCode
  assert_eq(unset_span.status, Unset)
  assert_eq(ok_span.status, Ok)
  assert_eq(error_span.status, Error)
  
  // 验证状态描述
  match unset_span.status_description {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
  
  match ok_span.status_description {
    Some(desc) => assert_eq(desc, "Operation succeeded")
    None => @test.fail("Test failed")
  }
  
  match error_span.status_description {
    Some(desc) => assert_eq(desc, "Operation failed")
    None => @test.fail("Test failed")
  }
}

test "span_event_and_link_creation" {
  // 测试SpanEvent和SpanLink的创建
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 3_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 3_byte })
  let span_context = SpanContext::{
    trace_id,
    span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 创建复杂的事件
  let error_event = SpanEvent::{
    name: "exception",
    timestamp_unix_nanos: 1640995200500000000L,
    attributes: [
      ("exception.type", AttributeValue::string("java.lang.NullPointerException")),
      ("exception.message", AttributeValue::string("Cannot invoke method on null object")),
      ("exception.stacktrace", AttributeValue::string("at com.example.Service.method(Service.java:123)")),
      ("exception.escaped", AttributeValue::bool(true))
    ]
  }
  
  // 创建复杂的链接
  let linked_context = SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 4_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 4_byte }),
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let causal_link = SpanLink::{
    context: linked_context,
    attributes: [
      ("link.type", AttributeValue::string("causal")),
      ("link.relationship", AttributeValue::string("parent")),
      ("link.trace_id", AttributeValue::string("04040404040404040404040404040404")),
      ("link.span_id", AttributeValue::string("0404040404040404"))
    ]
  }
  
  // 验证事件属性
  assert_eq(error_event.name, "exception")
  assert_eq(error_event.timestamp_unix_nanos, 1640995200500000000L)
  assert_eq(error_event.attributes.length(), 4)
  assert_eq(error_event.attributes[0].0, "exception.type")
  
  match error_event.attributes[0].1 {
    AttributeValue::StringValue(value) => assert_eq(value, "java.lang.NullPointerException")
    _ => @test.fail("Test failed")
  }
  
  // 验证链接属性
  assert_eq(causal_link.context.trace_id.length(), 16)
  assert_eq(causal_link.context.span_id.length(), 8)
  assert_eq(causal_link.attributes.length(), 4)
  assert_eq(causal_link.attributes[0].0, "link.type")
}

test "noop_tracer_provider_functionality" {
  // 测试NoopTracerProvider的功能
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test-tracer", Some("1.0.0"))
  
  // 测试创建不同类型的span
  let (ctx1, span1) = tracer.start_span(
    Context::empty(),
    "test-span-1",
    kind?: Internal,
    attributes?: [
      ("test.attribute", AttributeValue::string("test-value"))
    ],
    start_time_unix_nanos?: 1640995200000000000L
  )
  
  let (ctx2, span2) = tracer.start_span(
    ctx1,
    "test-span-2",
    kind?: Server,
    attributes?: [
      ("http.method", AttributeValue::string("POST")),
      ("http.url", AttributeValue::string("https://api.example.com/data"))
    ]
  )
  
  // 验证span属性
  assert_eq(span1.name, "test-span-1")
  assert_eq(span1.kind, Internal)
  assert_eq(span1.attributes.length(), 1)
  assert_eq(span1.start_time_unix_nanos, 1640995200000000000L)
  
  assert_eq(span2.name, "test-span-2")
  assert_eq(span2.kind, Server)
  assert_eq(span2.attributes.length(), 2)
  assert_eq(span2.start_time_unix_nanos, 0L) // 默认值
  
  // 验证上下文（Noop实现中上下文不会改变）
  assert_eq(ctx2.values.length(), ctx1.values.length())
}