test "span_kinds_and_status_codes" {
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer", "1.0.0")
  let ctx = Context::empty()
  
  // 测试所有SpanKind
  let (internal_ctx, internal_span) = tracer.start_span(ctx, "internal-span", Internal, [], 1000L)
  let (server_ctx, server_span) = tracer.start_span(ctx, "server-span", Server, [], 2000L)
  let (client_ctx, client_span) = tracer.start_span(ctx, "client-span", Client, [], 3000L)
  let (producer_ctx, producer_span) = tracer.start_span(ctx, "producer-span", Producer, [], 4000L)
  let (consumer_ctx, consumer_span) = tracer.start_span(ctx, "consumer-span", Consumer, [], 5000L)
  
  // 验证SpanKind
  match internal_span.kind {
    Internal => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match server_span.kind {
    Server => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match client_span.kind {
    Client => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match producer_span.kind {
    Producer => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match consumer_span.kind {
    Consumer => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  // 验证span名称和时间
  assert_eq(internal_span.name, "internal-span")
  assert_eq(server_span.name, "server-span")
  assert_eq(client_span.name, "client-span")
  assert_eq(producer_span.name, "producer-span")
  assert_eq(consumer_span.name, "consumer-span")
  
  assert_eq(internal_span.start_time_unix_nanos, 1000L)
  assert_eq(server_span.start_time_unix_nanos, 2000L)
  assert_eq(client_span.start_time_unix_nanos, 3000L)
  assert_eq(producer_span.start_time_unix_nanos, 4000L)
  assert_eq(consumer_span.start_time_unix_nanos, 5000L)
  
  // 测试所有StatusCode（通过手动创建span来测试不同的status）
  // 由于NoopTracer创建的span总是使用Unset状态，我们直接验证现有的span状态
  
  match internal_span.status {
    Unset => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match internal_span.status_description {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 测试带属性的span创建
  let attributes = [
    ("http.method", AttributeValue::string("GET")),
    ("http.status_code", AttributeValue::int(200L)),
    ("http.url", AttributeValue::string("https://example.com/api")),
    ("user.id", AttributeValue::string("user123")),
    ("request.duration", AttributeValue::float(123.45)),
    ("cache.hit", AttributeValue::bool(true)),
    ("tags", AttributeValue::array_string(["api", "v1", "public"])),
    ("retry.count", AttributeValue::int(3L)),
    ("timeout", AttributeValue::float(30.0)),
    ("success", AttributeValue::bool(true))
  ]
  
  let (attr_ctx, attr_span) = tracer.start_span(
    ctx,
    "span-with-attributes",
    Client,
    attributes,
    6000L
  )
  
  assert_eq(attr_span.name, "span-with-attributes")
  assert_eq(attr_span.attributes.length(), 10)
  
  // 验证各种属性类型
  match attr_span.attributes[0].1 {
    AttributeValue::StringValue(method) => assert_eq(method, "GET")
    _ => @test.fail("Test failed")
  }
  
  match attr_span.attributes[1].1 {
    AttributeValue::IntValue(status) => assert_eq(status, 200L)
    _ => @test.fail("Test failed")
  }
  
  match attr_span.attributes[2].1 {
    AttributeValue::StringValue(url) => assert_eq(url, "https://example.com/api")
    _ => @test.fail("Test failed")
  }
  
  match attr_span.attributes[3].1 {
    AttributeValue::StringValue(user_id) => assert_eq(user_id, "user123")
    _ => @test.fail("Test failed")
  }
  
  match attr_span.attributes[4].1 {
    AttributeValue::FloatValue(duration) => assert_eq(duration, 123.45)
    _ => @test.fail("Test failed")
  }
  
  match attr_span.attributes[5].1 {
    AttributeValue::BoolValue(cache_hit) => assert_eq(cache_hit, true)
    _ => @test.fail("Test failed")
  }
  
  match attr_span.attributes[6].1 {
    AttributeValue::ArrayStringValue(tags) => {
      assert_eq(tags.length(), 3)
      assert_eq(tags[0], "api")
      assert_eq(tags[1], "v1")
      assert_eq(tags[2], "public")
    }
    _ => @test.fail("Test failed")
  }
  
  // 测试特殊字符和Unicode的span名称
  let (special_ctx, special_span) = tracer.start_span(
    ctx,
    "special-span!@#$%^&*()",
    Internal,
    [],
    7000L
  )
  
  assert_eq(special_span.name, "special-span!@#$%^&*()")
  
  let (unicode_ctx, unicode_span) = tracer.start_span(
    ctx,
    "Unicode测试Span🚀",
    Server,
    [("unicode", AttributeValue::string("测试值🎉"))],
    8000L
  )
  
  assert_eq(unicode_span.name, "Unicode测试Span🚀")
  assert_eq(unicode_span.attributes.length(), 1)
  
  match unicode_span.attributes[0].1 {
    AttributeValue::StringValue(unicode_val) => assert_eq(unicode_val, "测试值🎉")
    _ => @test.fail("Test failed")
  }
}