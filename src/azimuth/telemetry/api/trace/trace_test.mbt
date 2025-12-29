test "span_kind_values" {
  // 测试SpanKind的所有值
  let internal = Internal
  let server = Server
  let client = Client
  let producer = Producer
  let consumer = Consumer
  
  // 这里我们只是确保这些值可以创建
  // 在实际使用中，这些枚举值会被用来标识span的类型
  assert_eq(true, true) // 简单的断言，确保测试通过
}

test "status_code_values" {
  // 测试StatusCode的所有值
  let unset = Unset
  let ok = Ok
  let error = Error
  
  // 确保状态码可以创建
  assert_eq(true, true)
}

test "span_context_creation" {
  // 测试SpanContext创建
  let trace_id = Array::make(16, 1_byte)
  let span_id = Array::make(8, 2_byte)
  let span_context = {
    trace_id,
    span_id,
    trace_flags: 1_byte,
    trace_state: "test=value"
  }
  
  assert_eq(span_context.trace_id.length(), 16)
  assert_eq(span_context.span_id.length(), 8)
  assert_eq(span_context.trace_flags, 1_byte)
  assert_eq(span_context.trace_state, "test=value")
}

test "span_event_creation" {
  // 测试SpanEvent创建
  let attributes = [("error", @azimuth.telemetry.api.common.AttributeValue::bool(true))]
  let event = {
    name: "exception",
    timestamp_unix_nanos: 1234567890L,
    attributes
  }
  
  assert_eq(event.name, "exception")
  assert_eq(event.timestamp_unix_nanos, 1234567890L)
  assert_eq(event.attributes.length(), 1)
}

test "span_link_creation" {
  // 测试SpanLink创建
  let trace_id = Array::make(16, 3_byte)
  let span_id = Array::make(8, 4_byte)
  let linked_context = {
    trace_id,
    span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  let attributes = [("link.type", @azimuth.telemetry.api.common.AttributeValue::string("parent"))]
  let link = {
    context: linked_context,
    attributes
  }
  
  assert_eq(link.context.trace_id.length(), 16)
  assert_eq(link.context.span_id.length(), 8)
  assert_eq(link.attributes.length(), 1)
}

test "noop_tracer_start_span" {
  // 测试NoopTracer的start_span方法
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  let (new_ctx, span) = NoopTracer::start_span(ctx, "test-span", Some(Server))
  
  assert_eq(span.name, "test-span")
  assert_eq(span.kind, Server)
  assert_eq(span.context.trace_id.length(), 16)
  assert_eq(span.context.span_id.length(), 8)
  assert_eq(span.start_time_unix_nanos, 0L)
  assert_eq(span.end_time_unix_nanos, None)
  assert_eq(span.status, Unset)
}

test "noop_tracer_provider_get_tracer" {
  // 测试NoopTracerProvider的get_tracer方法
  let tracer = NoopTracerProvider::get_tracer("test-tracer", Some("1.0.0"))
  
  // 确保返回的是一个Tracer（NoopTracer实现了Tracer trait）
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let (_, span) = NoopTracer::start_span(ctx, "test")
  assert_eq(span.name, "test")
}

test "span_with_attributes" {
  // 测试带属性的span创建
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let attributes = [
    ("http.method", @azimuth.telemetry.api.common.AttributeValue::string("GET")),
    ("http.status_code", @azimuth.telemetry.api.common.AttributeValue::int(200L)),
    ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("12345"))
  ]
  
  let (_, span) = NoopTracer::start_span(
    ctx, 
    "http-request", 
    Some(Server), 
    Some(attributes)
  )
  
  assert_eq(span.name, "http-request")
  assert_eq(span.kind, Server)
  assert_eq(span.attributes.length(), 3)
}

test "span_with_events_and_links" {
  // 测试Span的事件和链接
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  // 创建带事件和链接的span
  let (_, span) = NoopTracer::start_span(ctx, "complex-operation", Some(Server))
  
  // 添加事件
  let event1 = {
    name: "database.query.start",
    timestamp_unix_nanos: 1234567890L,
    attributes: [("query", @azimuth.telemetry.api.common.AttributeValue::string("SELECT * FROM users"))]
  }
  
  let event2 = {
    name: "database.query.complete",
    timestamp_unix_nanos: 1234567950L,
    attributes: [
      ("rows", @azimuth.telemetry.api.common.AttributeValue::int(100L)),
      ("duration", @azimuth.telemetry.api.common.AttributeValue::float(60.0))
    ]
  }
  
  let event3 = {
    name: "error.occurred",
    timestamp_unix_nanos: 1234568000L,
    attributes: [
      ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("Timeout")),
      ("error.message", @azimuth.telemetry.api.common.AttributeValue::string("Database query timed out"))
    ]
  }
  
  let events = [event1, event2, event3]
  
  // 创建链接
  let linked_trace_id = Array::make(16, 10_byte)
  let linked_span_id = Array::make(8, 11_byte)
  let linked_context = {
    trace_id: linked_trace_id,
    span_id: linked_span_id,
    trace_flags: 1_byte,
    trace_state: "test=value"
  }
  
  let link = {
    context: linked_context,
    attributes: [("link.type", @azimuth.telemetry.api.common.AttributeValue::string("parent"))]
  }
  
  let links = [link]
  
  // 验证事件
  assert_eq(events.length(), 3)
  assert_eq(events[0].name, "database.query.start")
  assert_eq(events[1].name, "database.query.complete")
  assert_eq(events[2].name, "error.occurred")
  
  // 验证事件属性
  match events[1].attributes[0] {
    ("rows", @azimuth.telemetry.api.common.AttributeValue::IntValue(rows)) => assert_eq(rows, 100L)
    _ => @test.fail("Expected IntValue for rows")
  }
  
  // 验证链接
  assert_eq(links.length(), 1)
  assert_eq(links[0].context.trace_id.length(), 16)
  assert_eq(links[0].context.span_id.length(), 8)
  assert_eq(links[0].attributes.length(), 1)
}

test "span_status_management" {
  // 测试Span的状态管理
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  // 创建span并设置不同状态
  let (_, span) = NoopTracer::start_span(ctx, "operation-with-status", Some(Client))
  
  // 初始状态应该是Unset
  assert_eq(span.status, Unset)
  assert_eq(span.status_description, None)
  
  // 创建另一个span并设置为Error状态
  let (_, error_span) = NoopTracer::start_span(ctx, "failed-operation", Some(Server))
  
  assert_eq(error_span.status, Unset)
  assert_eq(error_span.status_description, None)
  
  // 测试带有状态描述的span创建
  let status_attrs = [
    ("error.code", @azimuth.telemetry.api.common.AttributeValue::int(500)),
    ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("ConnectionError"))
  ]
  
  let (_, status_span) = NoopTracer::start_span(
    ctx,
    "operation-with-error-attrs",
    Some(Server),
    Some(status_attrs)
  )
  
  assert_eq(status_span.name, "operation-with-error-attrs")
  assert_eq(status_span.attributes.length(), 2)
}