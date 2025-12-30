// Trace模块Span生命周期测试
// 测试Span的创建、状态管理和各种操作

test "span_creation_with_default_values" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  let (new_ctx, span) = tracer.start_span(ctx, "test_span")
  
  assert_eq(span.name, "test_span")
  match span.kind {
    Internal => @test.succeed()
    _ => @test.fail("Expected Internal span kind")
  }
  assert_eq(span.parent_span_id, None)
  assert_eq(span.start_time_unix_nanos, 0L)
  assert_eq(span.end_time_unix_nanos, None)
  match span.status {
    Unset => @test.succeed()
    _ => @test.fail("Expected Unset status")
  }
  assert_eq(span.status_description, None)
  assert_eq(span.attributes.length(), 0)
  assert_eq(span.events.length(), 0)
  assert_eq(span.links.length(), 0)
}

test "span_creation_with_attributes" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  let attributes = [
    ("http.method", @azimuth.telemetry.api.common.AttributeValue::string("GET")),
    ("http.status_code", @azimuth.telemetry.api.common.AttributeValue::int(200L))
  ]
  let (new_ctx, span) = tracer.start_span(ctx, "http_request", kind: Some(Server), attributes: Some(attributes))
  
  assert_eq(span.name, "http_request")
  match span.kind {
    Server => @test.succeed()
    _ => @test.fail("Expected Server span kind")
  }
  assert_eq(span.attributes.length(), 2)
  match span.attributes[0] {
    (key, StringValue(value)) => {
      assert_eq(key, "http.method")
      assert_eq(value, "GET")
    }
    _ => @test.fail("Expected string attribute")
  }
  match span.attributes[1] {
    (key, IntValue(value)) => {
      assert_eq(key, "http.status_code")
      assert_eq(value, 200L)
    }
    _ => @test.fail("Expected int attribute")
  }
}

test "span_with_custom_start_time" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  let start_time = 1640995200000000000L  // 2022-01-01 00:00:00 UTC in nanoseconds
  let (new_ctx, span) = tracer.start_span(ctx, "timed_span", start_time_unix_nanos: Some(start_time))
  
  assert_eq(span.start_time_unix_nanos, start_time)
}

test "span_context_structure" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  let (new_ctx, span) = tracer.start_span(ctx, "context_test")
  
  assert_eq(span.context.trace_id.length(), 16)
  assert_eq(span.context.span_id.length(), 8)
  assert_eq(span.context.trace_flags, 0_byte)
  assert_eq(span.context.trace_state, "")
  
  // 验证所有字节都是0（NoopTracer的实现）
  let mut i = 0
  while i < span.context.trace_id.length() {
    assert_eq(span.context.trace_id[i], 0_byte)
    i = i + 1
  }
  
  i = 0
  while i < span.context.span_id.length() {
    assert_eq(span.context.span_id[i], 0_byte)
    i = i + 1
  }
}

test "span_kinds_coverage" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  
  let (_, internal_span) = tracer.start_span(ctx, "internal", kind: Some(Internal))
  let (_, server_span) = tracer.start_span(ctx, "server", kind: Some(Server))
  let (_, client_span) = tracer.start_span(ctx, "client", kind: Some(Client))
  let (_, producer_span) = tracer.start_span(ctx, "producer", kind: Some(Producer))
  let (_, consumer_span) = tracer.start_span(ctx, "consumer", kind: Some(Consumer))
  
  match internal_span.kind {
    Internal => @test.succeed()
    _ => @test.fail("Expected Internal")
  }
  match server_span.kind {
    Server => @test.succeed()
    _ => @test.fail("Expected Server")
  }
  match client_span.kind {
    Client => @test.succeed()
    _ => @test.fail("Expected Client")
  }
  match producer_span.kind {
    Producer => @test.succeed()
    _ => @test.fail("Expected Producer")
  }
  match consumer_span.kind {
    Consumer => @test.succeed()
    _ => @test.fail("Expected Consumer")
  }
}

test "tracer_provider_functionality" {
  let provider = NoopTracerProvider::{}
  let tracer1 = provider.get_tracer("tracer1", Some("1.0.0"))
  let tracer2 = provider.get_tracer("tracer2")
  
  // 验证返回的都是NoopTracer实例
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let (_, span1) = tracer1.start_span(ctx, "test1")
  let (_, span2) = tracer2.start_span(ctx, "test2")
  
  assert_eq(span1.name, "test1")
  assert_eq(span2.name, "test2")
}