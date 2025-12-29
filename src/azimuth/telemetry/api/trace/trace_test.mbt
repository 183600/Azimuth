test "span_creation_and_attributes" {
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer", "1.0.0")
  let ctx = Context::empty()
  
  let (new_ctx, span) = tracer.start_span(
    ctx,
    "test-span",
    Server,
    [("http.method", AttributeValue::string("GET"))],
    1234567890L
  )
  
  assert_eq(span.name, "test-span")
  match span.kind {
    Server => assert_eq(true, true)
    _ => @test.fail("Expected Server span kind")
  }
  assert_eq(span.start_time_unix_nanos, 1234567890L)
  assert_eq(span.attributes.length(), 1)
  
  match span.status {
    Unset => assert_eq(true, true)
    _ => @test.fail("Expected Unset status")
  }
}

test "span_context_details" {
  let span_context = SpanContext::{
    trace_id: [0_byte, 1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 
               8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte],
    span_id: [0_byte, 1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte],
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  assert_eq(span_context.trace_id.length(), 16)
  assert_eq(span_context.span_id.length(), 8)
  assert_eq(span_context.trace_flags, 1_byte)
  assert_eq(span_context.trace_state, "key1=value1,key2=value2")
  
  // 验证trace_id的具体字节
  assert_eq(span_context.trace_id[0], 0_byte)
  assert_eq(span_context.trace_id[15], 15_byte)
  
  // 验证span_id的具体字节
  assert_eq(span_context.span_id[0], 0_byte)
  assert_eq(span_context.span_id[7], 7_byte)
}

test "span_events_and_links_creation" {
  let event = SpanEvent::{
    name: "error",
    timestamp_unix_nanos: 1234567890L,
    attributes: [("exception.message", AttributeValue::string("Something went wrong"))]
  }
  
  let link_context = SpanContext::{
    trace_id: Array::make(16, 0_byte),
    span_id: Array::make(8, 1_byte),
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  let link = SpanLink::{
    context: link_context,
    attributes: [("link.type", AttributeValue::string("parent"))]
  }
  
  assert_eq(event.name, "error")
  assert_eq(event.timestamp_unix_nanos, 1234567890L)
  assert_eq(event.attributes.length(), 1)
  
  // 验证event属性
  match event.attributes[0].1 {
    AttributeValue::StringValue(msg) => assert_eq(msg, "Something went wrong")
    _ => @test.fail("Expected StringValue")
  }
  
  assert_eq(link.context.span_id[0], 1_byte)
  assert_eq(link.attributes.length(), 1)
  
  // 验证link属性
  match link.attributes[0].1 {
    AttributeValue::StringValue(link_type) => assert_eq(link_type, "parent")
    _ => @test.fail("Expected StringValue")
  }
}