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
    _ => @test.fail("Expected StringValue")
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
  let trace_id = Array::make(16, 0_byte)
  let span_id = Array::make(8, 0_byte)
  
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
  let linked_trace_id = Array::make(16, 1_byte)
  let linked_span_id = Array::make(8, 1_byte)
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
    _ => @test.fail("Expected StringValue")
  }
  
  // 验证链接属性
  assert_eq(span_link.context.trace_id.length(), 16)
  assert_eq(span_link.context.span_id.length(), 8)
  assert_eq(span_link.context.trace_flags, 1_byte)
  assert_eq(span_link.attributes.length(), 1)
  assert_eq(span_link.attributes[0].0, "link.type")
  
  match span_link.attributes[0].1 {
    StringValue(value) => assert_eq(value, "parent")
    _ => @test.fail("Expected StringValue")
  }
}