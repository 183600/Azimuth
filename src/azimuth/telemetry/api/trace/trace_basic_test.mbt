// Trace模块基本测试用例
test "span_creation" {
  let ctx = Context::empty()
  let (_, span) = NoopTracer::start_span(ctx, "test_span")
  
  assert_eq(span.name, "test_span")
  assert_eq(span.kind, Internal)
  assert_eq(span.status, Unset)
  assert_eq(span.parent_span_id, None)
  assert_eq(span.start_time_unix_nanos, 0L)
  assert_eq(span.end_time_unix_nanos, None)
  assert_eq(span.events.length(), 0)
  assert_eq(span.links.length(), 0)
}

test "span_with_attributes" {
  let ctx = Context::empty()
  let attributes = [("key1", AttributeValue::string("value1"))]
  let (_, span) = NoopTracer::start_span(ctx, "test_span", Internal, Some(attributes))
  
  assert_eq(span.name, "test_span")
  assert_eq(span.attributes.length(), 1)
  assert_eq(span.attributes[0].0, "key1")
  
  match span.attributes[0].1 {
    StringValue(value) => assert_eq(value, "value1")
    _ => @test.fail("Test failed")
  }
}

test "span_with_different_kinds" {
  let ctx = Context::empty()
  
  let (_, internal_span) = NoopTracer::start_span(ctx, "internal", Internal)
  assert_eq(internal_span.kind, Internal)
  
  let (_, server_span) = NoopTracer::start_span(ctx, "server", Server)
  assert_eq(server_span.kind, Server)
  
  let (_, client_span) = NoopTracer::start_span(ctx, "client", Client)
  assert_eq(client_span.kind, Client)
  
  let (_, producer_span) = NoopTracer::start_span(ctx, "producer", Producer)
  assert_eq(producer_span.kind, Producer)
  
  let (_, consumer_span) = NoopTracer::start_span(ctx, "consumer", Consumer)
  assert_eq(consumer_span.kind, Consumer)
}

test "tracer_provider" {
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("test_tracer", Some("1.0.0"))
  
  // No-op实现应该返回一个有效的tracer
  let ctx = Context::empty()
  let (_, span) = tracer.start_span(ctx, "test_span")
  assert_eq(span.name, "test_span")
}

test "span_context_properties" {
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte })
  
  let span_context = SpanContext::{
    trace_id,
    span_id,
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  assert_eq(span_context.trace_id.length(), 16)
  assert_eq(span_context.span_id.length(), 8)
  assert_eq(span_context.trace_flags, 1_byte)
  assert_eq(span_context.trace_state, "key1=value1,key2=value2")
  
  // 测试不同的trace_flags值
  let span_context_sampled = SpanContext::{
    trace_id,
    span_id,
    trace_flags: 1_byte,  // 采样标志
    trace_state: ""
  }
  
  let span_context_not_sampled = SpanContext::{
    trace_id,
    span_id,
    trace_flags: 0_byte,  // 不采样标志
    trace_state: ""
  }
  
  assert_eq(span_context_sampled.trace_flags, 1_byte)
  assert_eq(span_context_not_sampled.trace_flags, 0_byte)
}

test "span_events_and_links" {
  let ctx = Context::empty()
  let attributes = [("event.key", AttributeValue::string("event.value"))]
  let (_, span) = NoopTracer::start_span(ctx, "test_span", Internal, Some(attributes))
  
  // 创建span事件
  let event_attributes = [("error.type", AttributeValue::string("timeout"))]
  let span_event = SpanEvent::{
    name: "error_occurred",
    timestamp_unix_nanos: 1234567890L,
    attributes: event_attributes
  }
  
  // 创建span链接
  let linked_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })
  let linked_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte })
  let linked_context = SpanContext::{
    trace_id: linked_trace_id,
    span_id: linked_span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let link_attributes = [("link.type", AttributeValue::string("parent"))]
  let span_link = SpanLink::{
    context: linked_context,
    attributes: link_attributes
  }
  
  // 验证事件属性
  assert_eq(span_event.name, "error_occurred")
  assert_eq(span_event.timestamp_unix_nanos, 1234567890L)
  assert_eq(span_event.attributes.length(), 1)
  assert_eq(span_event.attributes[0].0, "error.type")
  
  match span_event.attributes[0].1 {
    StringValue(value) => assert_eq(value, "timeout")
    _ => @test.fail("Test failed")
  }
  
  // 验证链接属性
  assert_eq(span_link.context.trace_id.length(), 16)
  assert_eq(span_link.context.span_id.length(), 8)
  assert_eq(span_link.context.trace_flags, 1_byte)
  assert_eq(span_link.attributes.length(), 1)
  assert_eq(span_link.attributes[0].0, "link.type")
  
  match span_link.attributes[0].1 {
    StringValue(value) => assert_eq(value, "parent")
    _ => @test.fail("Test failed")
  }
}

test "span_with_complete_information" {
  let ctx = Context::empty()
  
  // 创建完整的SpanContext
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x0a_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x0b_byte })
  let parent_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x0c_byte })
  
  let span_context = SpanContext::{
    trace_id,
    span_id,
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  // 创建复杂的属性集合
  let complex_attributes = [
    ("service.name", AttributeValue::string("payment-service")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("http.method", AttributeValue::string("POST")),
    ("http.url", AttributeValue::string("https://api.example.com/payments")),
    ("http.status_code", AttributeValue::int(200L)),
    ("user.id", AttributeValue::string("user-12345")),
    ("payment.amount", AttributeValue::float(99.99)),
    ("payment.currency", AttributeValue::string("USD")),
    ("db.query.duration_ms", AttributeValue::int(150L)),
    ("cache.hit", AttributeValue::bool(true)),
    ("tags", AttributeValue::array_string(["api", "payment", "critical"])),
    ("status.codes", AttributeValue::array_int([200L, 201L, 400L])),
    ("response.sizes", AttributeValue::array_float([1024.0, 2048.0, 4096.0])),
    ("feature.flags", AttributeValue::array_bool([true, false, true]))
  ]
  
  // 创建多个事件
  let events = [
    SpanEvent::{
      name: "db.query.start",
      timestamp_unix_nanos: 1640995200000000000L,
      attributes: [
        ("db.statement", AttributeValue::string("SELECT * FROM payments WHERE id = ?")),
        ("db.type", AttributeValue::string("postgresql"))
      ]
    },
    SpanEvent::{
      name: "db.query.complete",
      timestamp_unix_nanos: 1640995200000000150L,
      attributes: [
        ("db.rows_affected", AttributeValue::int(1L)),
        ("db.duration_ms", AttributeValue::int(150L))
      ]
    },
    SpanEvent::{
      name: "cache.check",
      timestamp_unix_nanos: 1640995200000000100L,
      attributes: [
        ("cache.key", AttributeValue::string("user:12345:profile")),
        ("cache.hit", AttributeValue::bool(true))
      ]
    }
  ]
  
  // 创建多个链接
  let links = [
    SpanLink::{
      context: SpanContext::{
        trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
        span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte }),
        trace_flags: 1_byte,
        trace_state: ""
      },
      attributes: [
        ("link.type", AttributeValue::string("parent")),
        ("parent.service", AttributeValue::string("api-gateway"))
      ]
    },
    SpanLink::{
      context: SpanContext::{
        trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x03_byte }),
        span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x04_byte }),
        trace_flags: 0_byte,
        trace_state: "key=value"
      },
      attributes: [
        ("link.type", AttributeValue::string("related")),
        ("related.operation", AttributeValue::string("authentication"))
      ]
    }
  ]
  
  // 创建完整的Span
  let complete_span = Span::{
    name: "payment.process",
    context: span_context,
    kind: Server,
    parent_span_id: Some(parent_span_id),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200000000500L),
    status: Ok,
    status_description: Some("Payment processed successfully"),
    attributes: complex_attributes,
    events: events,
    links: links
  }
  
  // 验证Span属性
  assert_eq(complete_span.name, "payment.process")
  assert_eq(complete_span.kind, Server)
  assert_eq(complete_span.status, Ok)
  assert_eq(complete_span.start_time_unix_nanos, 1640995200000000000L)
  
  match complete_span.end_time_unix_nanos {
    Some(end_time) => assert_eq(end_time, 1640995200000000500L)
    None => @test.fail("Test failed")
  }
  
  match complete_span.status_description {
    Some(description) => assert_eq(description, "Payment processed successfully")
    None => @test.fail("Test failed")
  }
  
  match complete_span.parent_span_id {
    Some(parent_id) => {
      assert_eq(parent_id.length(), 8)
      assert_eq(parent_id[0], 0x0c_byte)
    }
    None => @test.fail("Test failed")
  }
  
  // 验证上下文
  assert_eq(complete_span.context.trace_id.length(), 16)
  assert_eq(complete_span.context.span_id.length(), 8)
  assert_eq(complete_span.context.trace_flags, 1_byte)
  assert_eq(complete_span.context.trace_state, "key1=value1,key2=value2")
  
  // 验证属性
  assert_eq(complete_span.attributes.length(), 14)
  
  // 验证事件
  assert_eq(complete_span.events.length(), 3)
  assert_eq(complete_span.events[0].name, "db.query.start")
  assert_eq(complete_span.events[1].name, "db.query.complete")
  assert_eq(complete_span.events[2].name, "cache.check")
  
  // 验证链接
  assert_eq(complete_span.links.length(), 2)
  assert_eq(complete_span.links[0].attributes.length(), 2)
  assert_eq(complete_span.links[1].attributes.length(), 2)
}

test "span_context_edge_cases_and_special_values" {
  // 测试边界情况和特殊值的SpanContext
  
  // 全零的trace_id和span_id
  let zero_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte })
  let zero_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte })
  
  let zero_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  // 全1的trace_id和span_id
  let max_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0xff_byte })
  let max_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0xff_byte })
  
  let max_context = SpanContext::{
    trace_id: max_trace_id,
    span_id: max_span_id,
    trace_flags: 0xff_byte,
    trace_state: ""
  }
  
  // 验证零值上下文
  assert_eq(zero_context.trace_id.length(), 16)
  assert_eq(zero_context.span_id.length(), 8)
  assert_eq(zero_context.trace_flags, 0_byte)
  assert_eq(zero_context.trace_state, "")
  
  // 验证所有字节都是0
  let mut i = 0
  while i < 16 {
    assert_eq(zero_context.trace_id[i], 0_byte)
    i = i + 1
  }
  
  i = 0
  while i < 8 {
    assert_eq(zero_context.span_id[i], 0_byte)
    i = i + 1
  }
  
  // 验证最大值上下文
  assert_eq(max_context.trace_id.length(), 16)
  assert_eq(max_context.span_id.length(), 8)
  assert_eq(max_context.trace_flags, 0xff_byte)
  assert_eq(max_context.trace_state, "")
  
  // 验证所有字节都是0xff
  i = 0
  while i < 16 {
    assert_eq(max_context.trace_id[i], 0xff_byte)
    i = i + 1
  }
  
  i = 0
  while i < 8 {
    assert_eq(max_context.span_id[i], 0xff_byte)
    i = i + 1
  }
  
  // 测试复杂的trace_state
  let complex_trace_state = "key1=value1,key2=value2;prop1=val1,key3=value3;prop1=val1;prop2=val2"
  let complex_context = SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x12_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x34_byte }),
    trace_flags: 1_byte,
    trace_state: complex_trace_state
  }
  
  assert_eq(complex_context.trace_state, complex_trace_state)
  assert_eq(complex_context.trace_flags, 1_byte)
  
  // 测试空trace_state
  let empty_state_context = SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x56_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x78_byte }),
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  assert_eq(empty_state_context.trace_state, "")
  assert_eq(empty_state_context.trace_flags, 0_byte)
}

test "span_status_and_lifecycle" {
  let ctx = Context::empty()
  
  // 创建一个span并测试其生命周期
  let (_, span) = NoopTracer::start_span(ctx, "lifecycle_test", Client, None)
  
  // 初始状态
  assert_eq(span.name, "lifecycle_test")
  assert_eq(span.kind, Client)
  assert_eq(span.status, Unset)
  match span.status_description {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 创建不同状态的span
  let unset_span = Span::{
    name: "unset_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  let ok_span = Span::{
    name: "ok_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(2000L),
    status: Ok,
    status_description: Some("Operation completed successfully"),
    attributes: [],
    events: [],
    links: []
  }
  
  let error_span = Span::{
    name: "error_span",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(1500L),
    status: Error,
    status_description: Some("Database connection failed"),
    attributes: [
      ("error.code", AttributeValue::string("DB_CONN_FAILED")),
      ("error.retries", AttributeValue::int(3L))
    ],
    events: [
      SpanEvent::{
        name: "error",
        timestamp_unix_nanos: 1200L,
        attributes: [
          ("error.type", AttributeValue::string("timeout")),
          ("error.message", AttributeValue::string("Connection timeout after 30 seconds"))
        ]
      }
    ],
    links: []
  }
  
  // 验证不同状态的span
  assert_eq(unset_span.status, Unset)
  match unset_span.status_description {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  match unset_span.end_time_unix_nanos {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  assert_eq(ok_span.status, Ok)
  match ok_span.status_description {
    Some(description) => assert_eq(description, "Operation completed successfully")
    None => @test.fail("Test failed")
  }
  match ok_span.end_time_unix_nanos {
    Some(end_time) => assert_eq(end_time, 2000L)
    None => @test.fail("Test failed")
  }
  
  assert_eq(error_span.status, Error)
  match error_span.status_description {
    Some(description) => assert_eq(description, "Database connection failed")
    None => @test.fail("Test failed")
  }
  match error_span.end_time_unix_nanos {
    Some(end_time) => assert_eq(end_time, 1500L)
    None => @test.fail("Test failed")
  }
  assert_eq(error_span.attributes.length(), 2)
  assert_eq(error_span.events.length(), 1)
}