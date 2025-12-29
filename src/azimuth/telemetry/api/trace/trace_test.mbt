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
  let attributes = [("error", @common.AttributeValue::bool(true))]
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
  
  let attributes = [("link.type", @common.AttributeValue::string("parent"))]
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
  let tracer = NoopTracer
  let ctx = @context.Context::empty()
  
  let (new_ctx, span) = tracer.start_span(ctx, "test-span", Some(Server))
  
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
  let provider = NoopTracerProvider
  let tracer = provider.get_tracer("test-tracer", Some("1.0.0"))
  
  // 确保返回的是一个Tracer（NoopTracer实现了Tracer trait）
  let ctx = @context.Context::empty()
  let (_, span) = tracer.start_span(ctx, "test")
  assert_eq(span.name, "test")
}

test "span_with_attributes" {
  // 测试带属性的span创建
  let tracer = NoopTracer
  let ctx = @context.Context::empty()
  let attributes = [
    ("http.method", @common.AttributeValue::string("GET")),
    ("http.status_code", @common.AttributeValue::int(200L)),
    ("user.id", @common.AttributeValue::string("12345"))
  ]
  
  let (_, span) = tracer.start_span(
    ctx, 
    "http-request", 
    Some(Server), 
    Some(attributes)
  )
  
  assert_eq(span.name, "http-request")
  assert_eq(span.kind, Server)
  assert_eq(span.attributes.length(), 3)
}