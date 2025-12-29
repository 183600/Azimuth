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