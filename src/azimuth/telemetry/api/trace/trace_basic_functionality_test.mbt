// Trace API基础功能测试
test "SpanKind enum values" {
  let internal_kind = Internal
  let server_kind = Server
  let client_kind = Client
  let producer_kind = Producer
  let consumer_kind = Consumer
  
  assert match (internal_kind) {
    Internal => true
    _ => false
  }
  
  assert match (server_kind) {
    Server => true
    _ => false
  }
  
  assert match (client_kind) {
    Client => true
    _ => false
  }
  
  assert match (producer_kind) {
    Producer => true
    _ => false
  }
  
  assert match (consumer_kind) {
    Consumer => true
    _ => false
  }
}

test "StatusCode enum values" {
  let unset_status = Unset
  let ok_status = Ok
  let error_status = Error
  
  assert match (unset_status) {
    Unset => true
    _ => false
  }
  
  assert match (ok_status) {
    Ok => true
    _ => false
  }
  
  assert match (error_status) {
    Error => true
    _ => false
  }
}

test "SpanContext creation and validation" {
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(i) { (i % 256) |> to_int64 |> Int64::to_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(i) { (i % 256) |> to_int64 |> Int64::to_byte })
  
  let span_context = SpanContext::{
    trace_id,
    span_id,
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  assert span_context.trace_id.length == 16
  assert span_context.span_id.length == 8
  assert span_context.trace_flags == 1_byte
  assert span_context.trace_state == "key1=value1,key2=value2"
}

test "Span creation with basic fields" {
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte })
  let span_context = SpanContext::{
    trace_id,
    span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  let span = Span::{
    name: "test-span",
    context: span_context,
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1234567890L,
    end_time_unix_nanos: Some(1234567891L),
    status: Ok,
    status_description: Some("Success"),
    attributes: [("http.method", AttributeValue::string("GET"))],
    events: [],
    links: []
  }
  
  assert span.name == "test-span"
  assert match (span.kind) {
    Server => true
    _ => false
  }
  assert span.start_time_unix_nanos == 1234567890L
  assert match (span.end_time_unix_nanos) {
    Some(time) => time == 1234567891L
    None => false
  }
  assert match (span.status) {
    Ok => true
    _ => false
  }
  assert span.attributes.length == 1
}

test "SpanEvent creation" {
  let event = SpanEvent::{
    name: "error-event",
    timestamp_unix_nanos: 1234567890L,
    attributes: [
      ("error.type", AttributeValue::string("timeout")),
      ("error.message", AttributeValue::string("Request timed out"))
    ]
  }
  
  assert event.name == "error-event"
  assert event.timestamp_unix_nanos == 1234567890L
  assert event.attributes.length == 2
}

test "SpanLink creation" {
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte })
  let linked_context = SpanContext::{
    trace_id,
    span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let link = SpanLink::{
    context: linked_context,
    attributes: [
      ("link.type", AttributeValue::string("parent")),
      ("link.strength", AttributeValue::float(0.8))
    ]
  }
  
  assert link.context.trace_id.length == 16
  assert link.context.span_id.length == 8
  assert link.attributes.length == 2
}

test "NoopTracer basic functionality" {
  let tracer = NoopTracer::{}
  let ctx = context::Context::empty()
  
  let (new_ctx, span) = tracer.start_span(
    ctx,
    "noop-span",
    kind: Internal,
    attributes: [("test.attr", AttributeValue::string("test-value"))]
  )
  
  assert span.name == "noop-span"
  assert match (span.kind) {
    Internal => true
    _ => false
  }
  assert span.attributes.length == 1
  assert span.context.trace_id.length == 16
  assert span.context.span_id.length == 8
}

test "NoopTracerProvider functionality" {
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer", version: Some("1.0.0"))
  
  // 验证返回的是NoopTracer实例
  let ctx = context::Context::empty()
  let (_, span) = tracer.start_span(ctx, "test-span")
  assert span.name == "test-span"
}