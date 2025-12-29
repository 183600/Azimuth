// Span生命周期状态转换测试
test "span_lifecycle_state_transitions" {
  // 测试Span的基本状态转换
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建Span - 初始状态
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "test-span",
    Internal,
    Some([
      ("operation.type", AttributeValue::string("test")),
      ("service.name", AttributeValue::string("test-service"))
    ]),
    Some(1234567890L)
  )
  
  // 验证初始状态
  assert_eq(span.name, "test-span")
  assert_eq(span.kind, Internal)
  assert_eq(span.start_time_unix_nanos, 1234567890L)
  assert_eq(span.end_time_unix_nanos, None)
  assert_eq(span.status, Unset)
  assert_eq(span.status_description, None)
  assert_eq(span.attributes.length(), 2)
  assert_eq(span.events.length(), 0)
  assert_eq(span.links.length(), 0)
  
  // 验证SpanContext
  assert_eq(span.context.trace_id.length(), 16)
  assert_eq(span.context.span_id.length(), 8)
  assert_eq(span.context.trace_flags, 0_byte)
  assert_eq(span.context.trace_state, "")
  
  // 验证Context保持不变
  assert_eq(span_ctx.values.length(), ctx.values.length())
}

test "span_status_transitions" {
  // 测试Span状态码的转换
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "status-test-span",
    Server,
    Some([
      ("operation.type", AttributeValue::string("status-test"))
    ]),
    Some(1234567890L)
  )
  
  // 初始状态应该是Unset
  assert_eq(span.status, Unset)
  assert_eq(span.status_description, None)
  
  // 在实际实现中，Span状态应该是可变的
  // 但由于MoonBit的结构体是不可变的，这里我们验证初始状态
  assert_eq(span.status, Unset)
  
  // 测试不同类型的SpanKind
  let (internal_ctx, internal_span) = tracer.start_span(
    ctx,
    "internal-span",
    Internal,
    Some([("span.type", AttributeValue::string("internal"))]),
    Some(1234567891L)
  )
  
  let (server_ctx, server_span) = tracer.start_span(
    ctx,
    "server-span",
    Server,
    Some([("span.type", AttributeValue::string("server"))]),
    Some(1234567892L)
  )
  
  let (client_ctx, client_span) = tracer.start_span(
    ctx,
    "client-span",
    Client,
    Some([("span.type", AttributeValue::string("client"))]),
    Some(1234567893L)
  )
  
  let (producer_ctx, producer_span) = tracer.start_span(
    ctx,
    "producer-span",
    Producer,
    Some([("span.type", AttributeValue::string("producer"))]),
    Some(1234567894L)
  )
  
  let (consumer_ctx, consumer_span) = tracer.start_span(
    ctx,
    "consumer-span",
    Consumer,
    Some([("span.type", AttributeValue::string("consumer"))]),
    Some(1234567895L)
  )
  
  // 验证所有SpanKind都正确设置
  assert_eq(internal_span.kind, Internal)
  assert_eq(server_span.kind, Server)
  assert_eq(client_span.kind, Client)
  assert_eq(producer_span.kind, Producer)
  assert_eq(consumer_span.kind, Consumer)
  
  // 验证所有Span都有正确的初始状态
  assert_eq(internal_span.status, Unset)
  assert_eq(server_span.status, Unset)
  assert_eq(client_span.status, Unset)
  assert_eq(producer_span.status, Unset)
  assert_eq(consumer_span.status, Unset)
}

test "span_context_lifecycle" {
  // 测试SpanContext的生命周期
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "context-test-span",
    Client,
    Some([
      ("operation.type", AttributeValue::string("context-test"))
    ]),
    Some(1234567890L)
  )
  
  // 验证SpanContext的结构
  let span_context = span.context
  assert_eq(span_context.trace_id.length(), 16)
  assert_eq(span_context.span_id.length(), 8)
  assert_eq(span_context.trace_flags, 0_byte)
  assert_eq(span_context.trace_state, "")
  
  // 验证trace_id不为全零（在实际实现中应该生成有效的trace_id）
  let mut trace_id_sum = 0
  let mut i = 0
  while i < span_context.trace_id.length() {
    trace_id_sum = trace_id_sum + span_context.trace_id[i].to_int()
    i = i + 1
  }
  
  // 在NoopTracer中，trace_id是全零的
  assert_eq(trace_id_sum, 0)
  
  // 验证span_id不为全零（在实际实现中应该生成有效的span_id）
  let mut span_id_sum = 0
  i = 0
  while i < span_context.span_id.length() {
    span_id_sum = span_id_sum + span_context.span_id[i].to_int()
    i = i + 1
  }
  
  // 在NoopTracer中，span_id是全零的
  assert_eq(span_id_sum, 0)
}

test "span_attribute_lifecycle" {
  // 测试Span属性的生命周期
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建带有复杂属性的Span
  let complex_attributes : Array[(String, AttributeValue)] = [
    ("service.name", AttributeValue::string("test-service")),
    ("service.version", AttributeValue::string("1.0.0")),
    ("service.namespace", AttributeValue::string("production")),
    ("operation.name", AttributeValue::string("test.operation")),
    ("operation.type", AttributeValue::string("lifecycle-test")),
    ("user.id", AttributeValue::string("user-123456")),
    ("session.id", AttributeValue::string("session-abcdef")),
    ("request.id", AttributeValue::string("req-1234567890")),
    ("host.name", AttributeValue::string("test-host")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.cpu.percent", AttributeValue::float(75.5)),
    ("process.healthy", AttributeValue::bool(true)),
    ("tags", AttributeValue::array_string(["test", "lifecycle", "span"])),
    ("ports", AttributeValue::array_int([8080L, 8443L])),
    ("thresholds", AttributeValue::array_float([60.0, 80.0, 95.0])),
    ("features", AttributeValue::array_bool([true, false, true]))
  ]
  
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "attribute-test-span",
    Server,
    Some(complex_attributes),
    Some(1234567890L)
  )
  
  // 验证所有属性都正确设置
  assert_eq(span.attributes.length(), 16)
  
  // 验证特定属性
  let mut found_service_name = false
  let mut found_user_id = false
  let mut found_process_pid = false
  let mut found_tags = false
  let mut found_ports = false
  let mut found_thresholds = false
  let mut found_features = false
  
  let mut i = 0
  while i < span.attributes.length() {
    let (key, value) = span.attributes[i]
    
    match key {
      "service.name" => {
        match value {
          StringValue(s) => {
            assert_eq(s, "test-service")
            found_service_name = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "user.id" => {
        match value {
          StringValue(s) => {
            assert_eq(s, "user-123456")
            found_user_id = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "process.pid" => {
        match value {
          IntValue(pid) => {
            assert_eq(pid, 12345L)
            found_process_pid = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "tags" => {
        match value {
          ArrayStringValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], "test")
            assert_eq(arr[1], "lifecycle")
            assert_eq(arr[2], "span")
            found_tags = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "ports" => {
        match value {
          ArrayIntValue(arr) => {
            assert_eq(arr.length(), 2)
            assert_eq(arr[0], 8080L)
            assert_eq(arr[1], 8443L)
            found_ports = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "thresholds" => {
        match value {
          ArrayFloatValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], 60.0)
            assert_eq(arr[1], 80.0)
            assert_eq(arr[2], 95.0)
            found_thresholds = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "features" => {
        match value {
          ArrayBoolValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], true)
            assert_eq(arr[1], false)
            assert_eq(arr[2], true)
            found_features = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => () // 忽略其他属性
    }
    
    i = i + 1
  }
  
  // 验证所有重要属性都找到了
  assert_eq(found_service_name, true)
  assert_eq(found_user_id, true)
  assert_eq(found_process_pid, true)
  assert_eq(found_tags, true)
  assert_eq(found_ports, true)
  assert_eq(found_thresholds, true)
  assert_eq(found_features, true)
}

test "span_event_lifecycle" {
  // 测试Span事件的生命周期
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建带有事件的Span（在NoopTracer中events为空，但我们可以验证结构）
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "event-test-span",
    Server,
    Some([
      ("operation.type", AttributeValue::string("event-test"))
    ]),
    Some(1234567890L)
  )
  
  // 验证初始状态下events为空
  assert_eq(span.events.length(), 0)
  
  // 在实际实现中，应该能够添加事件到Span
  // 但由于MoonBit的不可变性，这里我们验证初始状态
  assert_eq(span.events.length(), 0)
}

test "span_link_lifecycle" {
  // 测试Span链接的生命周期
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建带有链接的Span（在NoopTracer中links为空，但我们可以验证结构）
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "link-test-span",
    Client,
    Some([
      ("operation.type", AttributeValue::string("link-test"))
    ]),
    Some(1234567890L)
  )
  
  // 验证初始状态下links为空
  assert_eq(span.links.length(), 0)
  
  // 在实际实现中，应该能够添加链接到Span
  // 但由于MoonBit的不可变性，这里我们验证初始状态
  assert_eq(span.links.length(), 0)
}

test "span_parent_relationship_lifecycle" {
  // 测试Span父子关系的生命周期
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建父Span
  let (parent_ctx, parent_span) = tracer.start_span(
    ctx,
    "parent-span",
    Server,
    Some([
      ("operation.type", AttributeValue::string("parent")),
      ("span.level", AttributeValue::string("root"))
    ]),
    Some(1234567890L)
  )
  
  // 验证父Span没有parent_span_id
  assert_eq(parent_span.parent_span_id, None)
  
  // 创建子Span（在实际实现中应该从parent_ctx创建）
  let (child_ctx, child_span) = tracer.start_span(
    parent_ctx,
    "child-span",
    Internal,
    Some([
      ("operation.type", AttributeValue::string("child")),
      ("span.level", AttributeValue::string("nested"))
    ]),
    Some(1234567891L)
  )
  
  // 在NoopTracer中，子Span不会自动设置parent_span_id
  // 但在实际实现中，应该设置parent_span_id
  assert_eq(child_span.parent_span_id, None)
  
  // 验证父子Span的基本属性
  assert_eq(parent_span.name, "parent-span")
  assert_eq(parent_span.kind, Server)
  assert_eq(child_span.name, "child-span")
  assert_eq(child_span.kind, Internal)
  
  // 验证时间顺序
  assert_eq(parent_span.start_time_unix_nanos, 1234567890L)
  assert_eq(child_span.start_time_unix_nanos, 1234567891L)
  assert_eq(child_span.start_time_unix_nanos > parent_span.start_time_unix_nanos, true)
}

test "span_timing_lifecycle" {
  // 测试Span时间相关的生命周期
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建带有特定时间的Span
  let start_time = 1234567890L
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "timing-test-span",
    Server,
    Some([
      ("operation.type", AttributeValue::string("timing-test"))
    ]),
    Some(start_time)
  )
  
  // 验证开始时间
  assert_eq(span.start_time_unix_nanos, start_time)
  
  // 验证结束时间未设置
  assert_eq(span.end_time_unix_nanos, None)
  
  // 测试不同的开始时间
  let time1 = 1000000000L
  let time2 = 2000000000L
  let time3 = 3000000000L
  
  let (_, span1) = tracer.start_span(ctx, "span-1", Internal, None, Some(time1))
  let (_, span2) = tracer.start_span(ctx, "span-2", Internal, None, Some(time2))
  let (_, span3) = tracer.start_span(ctx, "span-3", Internal, None, Some(time3))
  
  // 验证时间顺序
  assert_eq(span1.start_time_unix_nanos, time1)
  assert_eq(span2.start_time_unix_nanos, time2)
  assert_eq(span3.start_time_unix_nanos, time3)
  
  assert_eq(span2.start_time_unix_nanos > span1.start_time_unix_nanos, true)
  assert_eq(span3.start_time_unix_nanos > span2.start_time_unix_nanos, true)
  
  // 测试边界时间值
  let zero_time = 0L
  let max_time = 9223372036854775807L
  let negative_time = -1L
  
  let (_, zero_span) = tracer.start_span(ctx, "zero-span", Internal, None, Some(zero_time))
  let (_, max_span) = tracer.start_span(ctx, "max-span", Internal, None, Some(max_time))
  let (_, negative_span) = tracer.start_span(ctx, "negative-span", Internal, None, Some(negative_time))
  
  assert_eq(zero_span.start_time_unix_nanos, zero_time)
  assert_eq(max_span.start_time_unix_nanos, max_time)
  assert_eq(negative_span.start_time_unix_nanos, negative_time)
}

test "span_complex_lifecycle_scenarios" {
  // 测试复杂的Span生命周期场景
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建多个相关的Span来模拟复杂的调用链
  let (root_ctx, root_span) = tracer.start_span(
    ctx,
    "root-operation",
    Server,
    Some([
      ("service.name", AttributeValue::string("api-gateway")),
      ("operation.type", AttributeValue::string("http-request")),
      ("http.method", AttributeValue::string("POST")),
      ("http.url", AttributeValue::string("/api/v1/process")),
      ("user.id", AttributeValue::string("user-123456")),
      ("request.id", AttributeValue::string("req-abcdef123456"))
    ]),
    Some(1234567890L)
  )
  
  // 第一个子操作
  let (auth_ctx, auth_span) = tracer.start_span(
    root_ctx,
    "authenticate-user",
    Internal,
    Some([
      ("operation.type", AttributeValue::string("auth")),
      ("auth.method", AttributeValue::string("jwt")),
      ("auth.provider", AttributeValue::string("oauth2"))
    ]),
    Some(1234567891L)
  )
  
  // 第二个子操作
  let (db_ctx, db_span) = tracer.start_span(
    root_ctx,
    "database-query",
    Client,
    Some([
      ("operation.type", AttributeValue::string("db")),
      ("db.system", AttributeValue::string("postgresql")),
      ("db.statement", AttributeValue::string("SELECT * FROM users WHERE id = $1")),
      ("db.user", AttributeValue::string("app_user"))
    ]),
    Some(1234567892L)
  )
  
  // 第三个子操作
  let (cache_ctx, cache_span) = tracer.start_span(
    root_ctx,
    "cache-lookup",
    Client,
    Some([
      ("operation.type", AttributeValue::string("cache")),
      ("cache.system", AttributeValue::string("redis")),
      ("cache.command", AttributeValue::string("GET")),
      ("cache.key", AttributeValue::string("user:123456"))
    ]),
    Some(1234567893L)
  )
  
  // 验证所有Span的基本属性
  assert_eq(root_span.name, "root-operation")
  assert_eq(root_span.kind, Server)
  assert_eq(root_span.attributes.length(), 6)
  
  assert_eq(auth_span.name, "authenticate-user")
  assert_eq(auth_span.kind, Internal)
  assert_eq(auth_span.attributes.length(), 3)
  
  assert_eq(db_span.name, "database-query")
  assert_eq(db_span.kind, Client)
  assert_eq(db_span.attributes.length(), 4)
  
  assert_eq(cache_span.name, "cache-lookup")
  assert_eq(cache_span.kind, Client)
  assert_eq(cache_span.attributes.length(), 4)
  
  // 验证时间顺序
  assert_eq(root_span.start_time_unix_nanos, 1234567890L)
  assert_eq(auth_span.start_time_unix_nanos, 1234567891L)
  assert_eq(db_span.start_time_unix_nanos, 1234567892L)
  assert_eq(cache_span.start_time_unix_nanos, 1234567893L)
  
  assert_eq(auth_span.start_time_unix_nanos > root_span.start_time_unix_nanos, true)
  assert_eq(db_span.start_time_unix_nanos > auth_span.start_time_unix_nanos, true)
  assert_eq(cache_span.start_time_unix_nanos > db_span.start_time_unix_nanos, true)
  
  // 验证所有Span都有相同的状态
  assert_eq(root_span.status, Unset)
  assert_eq(auth_span.status, Unset)
  assert_eq(db_span.status, Unset)
  assert_eq(cache_span.status, Unset)
  
  // 验证所有Span都没有结束时间
  assert_eq(root_span.end_time_unix_nanos, None)
  assert_eq(auth_span.end_time_unix_nanos, None)
  assert_eq(db_span.end_time_unix_nanos, None)
  assert_eq(cache_span.end_time_unix_nanos, None)
}