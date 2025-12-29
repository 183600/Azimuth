// SpanContext高级功能测试
test "span_context_creation_and_validation" {
  // 测试SpanContext的基本创建和验证
  
  let trace_id = Array::make(16, 0_byte)
  trace_id[0] = 0x0a_byte
  trace_id[1] = 0xf7_byte
  trace_id[2] = 0x65_byte
  trace_id[3] = 0x19_byte
  
  let span_id = Array::make(8, 0_byte)
  span_id[0] = 0xb7_byte
  span_id[1] = 0xad_byte
  span_id[2] = 0x6b_byte
  span_id[3] = 0x71_byte
  
  let span_context = SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 0x01_byte,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 验证trace_id长度
  assert_eq(span_context.trace_id.length(), 16)
  assert_eq(span_context.span_id.length(), 8)
  assert_eq(span_context.trace_flags, 0x01_byte)
  assert_eq(span_context.trace_state.has_prefix("rojo"), true)
  assert_eq(span_context.trace_state.contains("congo"), true)
}

test "span_context_with_noop_tracer" {
  // 测试使用NoopTracer创建Span和SpanContext
  
  let ctx = context::Context::current() // 假设有current方法
  let (new_ctx, span) = NoopTracer::start_span(
    ctx,
    "test-span",
    Some(Server),
    Some([("http.method", AttributeValue::string("GET"))]),
    Some(1234567890L)
  )
  
  // 验证Span的基本属性
  assert_eq(span.name, "test-span")
  match span.kind {
    Server => assert_eq(true, true)
    _ => @test.fail("Expected Server span kind")
  }
  
  // 验证SpanContext
  assert_eq(span.context.trace_id.length(), 16)
  assert_eq(span.context.span_id.length(), 8)
  assert_eq(span.context.trace_flags, 0x00_byte)
  assert_eq(span.context.trace_state, "")
  
  // 验证属性
  assert_eq(span.attributes.length(), 1)
  assert_eq(span.attributes[0].0, "http.method")
  match span.attributes[0].1 {
    StringValue(value) => assert_eq(value, "GET")
    _ => @test.fail("Expected StringValue")
  }
}

test "span_lifecycle_with_events_and_links" {
  // 测试Span的生命周期，包括事件和链接
  
  let trace_id = Array::make(16, 0x01_byte)
  let span_id = Array::make(8, 0x02_byte)
  let parent_span_id = Array::make(8, 0x03_byte)
  
  let span_context = SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 0x01_byte,
    trace_state: "test=value"
  }
  
  let linked_context = SpanContext::{
    trace_id: Array::make(16, 0x04_byte),
    span_id: Array::make(8, 0x05_byte),
    trace_flags: 0x00_byte,
    trace_state: ""
  }
  
  let span = Span::{
    name: "lifecycle-test",
    context: span_context,
    kind: Client,
    parent_span_id: Some(parent_span_id),
    start_time_unix_nanos: 1234567890L,
    end_time_unix_nanos: Some(1234567990L),
    status: Ok,
    status_description: Some("Success"),
    attributes: [
      ("user.id", AttributeValue::int(12345L)),
      ("operation.type", AttributeValue::string("query"))
    ],
    events: [
      SpanEvent::{
        name: "database.query.start",
        timestamp_unix_nanos: 1234567950L,
        attributes: [
          ("db.statement", AttributeValue::string("SELECT * FROM users"))
        ]
      },
      SpanEvent::{
        name: "database.query.complete",
        timestamp_unix_nanos: 1234567980L,
        attributes: [
          ("db.rows_affected", AttributeValue::int(10L))
        ]
      }
    ],
    links: [
      SpanLink::{
        context: linked_context,
        attributes: [
          ("link.type", AttributeValue::string("follows-from"))
        ]
      }
    ]
  }
  
  // 验证Span基本属性
  assert_eq(span.name, "lifecycle-test")
  match span.kind {
    Client => assert_eq(true, true)
    _ => @test.fail("Expected Client span kind")
  }
  
  // 验证时间戳
  assert_eq(span.start_time_unix_nanos, 1234567890L)
  match span.end_time_unix_nanos {
    Some(end_time) => assert_eq(end_time, 1234567990L)
    None => @test.fail("Expected end time")
  }
  
  // 验证状态
  match span.status {
    Ok => assert_eq(true, true)
    _ => @test.fail("Expected Ok status")
  }
  
  // 验证属性
  assert_eq(span.attributes.length(), 2)
  assert_eq(span.attributes[0].0, "user.id")
  assert_eq(span.attributes[1].0, "operation.type")
  
  // 验证事件
  assert_eq(span.events.length(), 2)
  assert_eq(span.events[0].name, "database.query.start")
  assert_eq(span.events[1].name, "database.query.complete")
  assert_eq(span.events[0].attributes.length(), 1)
  assert_eq(span.events[1].attributes.length(), 1)
  
  // 验证链接
  assert_eq(span.links.length(), 1)
  assert_eq(span.links[0].attributes.length(), 1)
  assert_eq(span.links[0].context.trace_id.length(), 16)
  assert_eq(span.links[0].context.span_id.length(), 8)
}

test "span_kind_and_status_enums" {
  // 测试SpanKind和StatusCode枚举值
  
  // 测试所有SpanKind
  let internal_span = NoopTracer::start_span(
    context::Context::current(), "internal", Some(Internal), [], None
  ).1
  match internal_span.kind {
    Internal => assert_eq(true, true)
    _ => @test.fail("Expected Internal")
  }
  
  let server_span = NoopTracer::start_span(
    context::Context::current(), "server", Some(Server), [], None
  ).1
  match server_span.kind {
    Server => assert_eq(true, true)
    _ => @test.fail("Expected Server")
  }
  
  let client_span = NoopTracer::start_span(
    context::Context::current(), "client", Some(Client), [], None
  ).1
  match client_span.kind {
    Client => assert_eq(true, true)
    _ => @test.fail("Expected Client")
  }
  
  let producer_span = NoopTracer::start_span(
    context::Context::current(), "producer", Some(Producer), [], None
  ).1
  match producer_span.kind {
    Producer => assert_eq(true, true)
    _ => @test.fail("Expected Producer")
  }
  
  let consumer_span = NoopTracer::start_span(
    context::Context::current(), "consumer", Some(Consumer), [], None
  ).1
  match consumer_span.kind {
    Consumer => assert_eq(true, true)
    _ => @test.fail("Expected Consumer")
  }
}

test "span_context_trace_flags_and_state" {
  // 测试SpanContext的trace_flags和trace_state
  
  // 测试不同的trace_flags
  let sampled_context = SpanContext::{
    trace_id: Array::make(16, 0x01_byte),
    span_id: Array::make(8, 0x02_byte),
    trace_flags: 0x01_byte, // 采样标志
    trace_state: ""
  }
  assert_eq(sampled_context.trace_flags, 0x01_byte)
  
  let unsampled_context = SpanContext::{
    trace_id: Array::make(16, 0x01_byte),
    span_id: Array::make(8, 0x02_byte),
    trace_flags: 0x00_byte, // 未采样
    trace_state: ""
  }
  assert_eq(unsampled_context.trace_flags, 0x00_byte)
  
  // 测试复杂的trace_state
  let complex_trace_state = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE,custom=value"
  let complex_context = SpanContext::{
    trace_id: Array::make(16, 0x01_byte),
    span_id: Array::make(8, 0x02_byte),
    trace_flags: 0x01_byte,
    trace_state: complex_trace_state
  }
  
  assert_eq(complex_context.trace_state.contains("rojo"), true)
  assert_eq(complex_context.trace_state.contains("congo"), true)
  assert_eq(complex_context.trace_state.contains("custom"), true)
  
  // 验证trace_state格式
  let state_parts = complex_trace_state.split(",")
  assert_eq(state_parts.length(), 3)
  assert_eq(state_parts[0].has_prefix("rojo="), true)
  assert_eq(state_parts[1].has_prefix("congo="), true)
  assert_eq(state_parts[2].has_prefix("custom="), true)
}