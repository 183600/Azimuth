test "span_lifecycle_and_operations" {
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer", "1.0.0")
  let ctx = Context::empty()
  
  // 测试span的基本生命周期
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "lifecycle-test-span",
    Server,
    [("operation.type", AttributeValue::string("lifecycle"))],
    1234567890L
  )
  
  // 验证span的初始状态
  assert_eq(span.name, "lifecycle-test-span")
  match span.kind {
    Server => assert_eq(true, true)
    _ => @test.fail("Expected Server span kind")
  }
  assert_eq(span.start_time_unix_nanos, 1234567890L)
  match span.end_time_unix_nanos {
    Some(_) => @test.fail("Expected None for end_time")
    None => assert_eq(true, true)
  }
  match span.status {
    Unset => assert_eq(true, true)
    _ => @test.fail("Expected Unset status")
  }
  match span.status_description {
    Some(_) => @test.fail("Expected None for status_description")
    None => assert_eq(true, true)
  }
  assert_eq(span.attributes.length(), 1)
  assert_eq(span.events.length(), 0)
  assert_eq(span.links.length(), 0)
  
  // 测试span的context
  let span_context = span.context
  assert_eq(span_context.trace_id.length(), 16)
  assert_eq(span_context.span_id.length(), 8)
  assert_eq(span_context.trace_flags, 0_byte)
  assert_eq(span_context.trace_state, "")
  
  // 验证trace_id和span_id的具体值（应该都是0，因为NoopTracer使用零值）
  let mut i = 0
  while i < 16 {
    assert_eq(span_context.trace_id[i], 0_byte)
    i = i + 1
  }
  
  i = 0
  while i < 8 {
    assert_eq(span_context.span_id[i], 0_byte)
    i = i + 1
  }
  
  // 测试带有父span的span创建
  let parent_span_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]
  
  // 注意：当前NoopTracer实现不支持设置parent_span_id，但我们可以测试相关的字段
  let (child_ctx, child_span) = tracer.start_span(
    span_ctx,
    "child-span",
    Client,
    [("parent.child", AttributeValue::string("relationship"))],
    1234567891L
  )
  
  assert_eq(child_span.name, "child-span")
  match child_span.kind {
    Client => assert_eq(true, true)
    _ => @test.fail("Expected Client span kind")
  }
  assert_eq(child_span.start_time_unix_nanos, 1234567891L)
  
  // 测试span的事件和链接（通过直接创建来测试结构）
  let test_event = SpanEvent::{
    name: "test-event",
    timestamp_unix_nanos: 1234567892L,
    attributes: [("event.type", AttributeValue::string("test"))]
  }
  
  let test_link_context = SpanContext::{
    trace_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 
               9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte],
    span_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte],
    trace_flags: 1_byte,
    trace_state: "test=value"
  }
  
  let test_link = SpanLink::{
    context: test_link_context,
    attributes: [("link.type", AttributeValue::string("test-link"))]
  }
  
  // 验证event结构
  assert_eq(test_event.name, "test-event")
  assert_eq(test_event.timestamp_unix_nanos, 1234567892L)
  assert_eq(test_event.attributes.length(), 1)
  
  match test_event.attributes[0].1 {
    AttributeValue::StringValue(event_type) => assert_eq(event_type, "test")
    _ => @test.fail("Expected StringValue")
  }
  
  // 验证link结构
  assert_eq(test_link.context.trace_id.length(), 16)
  assert_eq(test_link.context.span_id.length(), 8)
  assert_eq(test_link.context.trace_flags, 1_byte)
  assert_eq(test_link.context.trace_state, "test=value")
  assert_eq(test_link.attributes.length(), 1)
  
  match test_link.attributes[0].1 {
    AttributeValue::StringValue(link_type) => assert_eq(link_type, "test-link")
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试不同时间戳的span
  let (past_ctx, past_span) = tracer.start_span(ctx, "past-span", Internal, [], -1000L)
  let (future_ctx, future_span) = tracer.start_span(ctx, "future-span", Internal, [], 9999999999999L)
  
  assert_eq(past_span.start_time_unix_nanos, -1000L)
  assert_eq(future_span.start_time_unix_nanos, 9999999999999L)
  
  // 测试零时间戳
  let (zero_ctx, zero_span) = tracer.start_span(ctx, "zero-span", Internal, [], 0L)
  assert_eq(zero_span.start_time_unix_nanos, 0L)
  
  // 测试最大和最小时间戳
  let max_time = 9223372036854775807L // Int64最大值
  let min_time = -9223372036854775808L // Int64最小值
  
  let (max_ctx, max_span) = tracer.start_span(ctx, "max-span", Internal, [], max_time)
  let (min_ctx, min_span) = tracer.start_span(ctx, "min-span", Internal, [], min_time)
  
  assert_eq(max_span.start_time_unix_nanos, max_time)
  assert_eq(min_span.start_time_unix_nanos, min_time)
  
  assert_eq(true, true) // 如果没有崩溃，测试通过
}