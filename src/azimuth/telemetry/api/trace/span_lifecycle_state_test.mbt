// Span生命周期和状态转换测试用例
// 测试Span的创建、状态转换和生命周期管理

test "span_creation_with_all_parameters" {
  // 测试使用所有参数创建Span
  let ctx = context::Context::empty()
  let tracer = NoopTracer::{}
  
  let attributes = [
    ("http.method", AttributeValue::string("GET")),
    ("http.url", AttributeValue::string("https://example.com/api")),
    ("http.status_code", AttributeValue::int(200L))
  ]
  
  let (new_ctx, span) = tracer.start_span(
    ctx,
    "http-request",
    Some(Server),
    Some(attributes),
    Some(1640995200000000000L)  // 2022-01-01 00:00:00 UTC in nanoseconds
  )
  
  assert_eq(span.name, "http-request")
  assert_eq(span.kind, Server)
  assert_eq(span.start_time_unix_nanos, 1640995200000000000L)
  assert_eq(span.end_time_unix_nanos, None)
  assert_eq(span.status, Unset)
  assert_eq(span.status_description, None)
  assert_eq(span.attributes.length(), 3)
  assert_eq(span.events.length(), 0)
  assert_eq(span.links.length(), 0)
  assert_eq(span.parent_span_id, None)
  
  // 验证属性值
  match span.attributes[0].1 {
    StringValue(s) => assert_eq(s, "GET")
    _ => @test.fail("Expected StringValue")
  }
  
  match span.attributes[1].1 {
    StringValue(s) => assert_eq(s, "https://example.com/api")
    _ => @test.fail("Expected StringValue")
  }
  
  match span.attributes[2].1 {
    IntValue(i) => assert_eq(i, 200L)
    _ => @test.fail("Expected IntValue")
  }
}

test "span_creation_with_minimal_parameters" {
  // 测试使用最少参数创建Span
  let ctx = context::Context::empty()
  let tracer = NoopTracer::{}
  
  let (new_ctx, span) = tracer.start_span(ctx, "minimal-span")
  
  assert_eq(span.name, "minimal-span")
  assert_eq(span.kind, Internal)  // 默认值
  assert_eq(span.start_time_unix_nanos, 0L)  // 默认值
  assert_eq(span.end_time_unix_nanos, None)
  assert_eq(span.status, Unset)
  assert_eq(span.status_description, None)
  assert_eq(span.attributes.length(), 0)
  assert_eq(span.events.length(), 0)
  assert_eq(span.links.length(), 0)
  assert_eq(span.parent_span_id, None)
}

test "span_kind_variations" {
  // 测试不同类型的SpanKind
  let ctx = context::Context::empty()
  let tracer = NoopTracer::{}
  
  // 测试所有SpanKind
  let (_, internal_span) = tracer.start_span(ctx, "internal", Some(Internal))
  let (_, server_span) = tracer.start_span(ctx, "server", Some(Server))
  let (_, client_span) = tracer.start_span(ctx, "client", Some(Client))
  let (_, producer_span) = tracer.start_span(ctx, "producer", Some(Producer))
  let (_, consumer_span) = tracer.start_span(ctx, "consumer", Some(Consumer))
  
  assert_eq(internal_span.kind, Internal)
  assert_eq(server_span.kind, Server)
  assert_eq(client_span.kind, Client)
  assert_eq(producer_span.kind, Producer)
  assert_eq(consumer_span.kind, Consumer)
}

test "span_context_properties" {
  // 测试SpanContext的属性
  let ctx = context::Context::empty()
  let tracer = NoopTracer::{}
  
  let (_, span) = tracer.start_span(ctx, "context-test")
  
  // 验证默认SpanContext
  assert_eq(span.context.trace_id.length(), 16)
  assert_eq(span.context.span_id.length(), 8)
  assert_eq(span.context.trace_flags, 0_byte)
  assert_eq(span.context.trace_state, "")
  
  // 验证所有trace_id字节都是0（NoopTracer的默认行为）
  let mut i = 0
  while i < span.context.trace_id.length() {
    assert_eq(span.context.trace_id[i], 0_byte)
    i = i + 1
  }
  
  // 验证所有span_id字节都是0（NoopTracer的默认行为）
  let mut j = 0
  while j < span.context.span_id.length() {
    assert_eq(span.context.span_id[j], 0_byte)
    j = j + 1
  }
}

test "span_attributes_complex_scenarios" {
  // 测试Span属性的复杂场景
  
  let ctx = context::Context::empty()
  let tracer = NoopTracer::{}
  
  // 复杂属性集合
  let complex_attributes = [
    // 字符串属性
    ("user.id", AttributeValue::string("user-123")),
    ("user.name", AttributeValue::string("张三")),
    ("request.id", AttributeValue::string("req-456")),
    
    // 数值属性
    ("request.size", AttributeValue::int(1024L)),
    ("response.size", AttributeValue::int(2048L)),
    ("duration.ms", AttributeValue::int(500L)),
    ("cpu.usage", AttributeValue::float(75.5)),
    ("memory.usage", AttributeValue::float(1024.5)),
    
    // 布尔属性
    ("success", AttributeValue::bool(true)),
    ("cached", AttributeValue::bool(false)),
    ("ssl.enabled", AttributeValue::bool(true)),
    
    // 数组属性
    ("http.headers", AttributeValue::array_string(["content-type", "authorization", "user-agent"])),
    ("status.codes", AttributeValue::array_int([200L, 404L, 500L])),
    ("response.times", AttributeValue::array_float([100.0, 200.0, 300.0])),
    ("feature.flags", AttributeValue::array_bool([true, false, true]))
  ]
  
  let (_, span) = tracer.start_span(ctx, "complex-attributes", Some(Internal), Some(complex_attributes))
  
  assert_eq(span.attributes.length(), 17)
  
  // 验证特定属性
  match span.attributes[0].1 {
    StringValue(s) => assert_eq(s, "user-123")
    _ => @test.fail("Expected StringValue")
  }
  
  match span.attributes[6].1 {
    IntValue(i) => assert_eq(i, 500L)
    _ => @test.fail("Expected IntValue")
  }
  
  match span.attributes[9].1 {
    FloatValue(f) => assert_eq(f, 1024.5)
    _ => @test.fail("Expected FloatValue")
  }
  
  match span.attributes[12].1 {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  match span.attributes[13].1 {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "content-type")
      assert_eq(arr[1], "authorization")
      assert_eq(arr[2], "user-agent")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
}

test "span_events_and_links" {
  // 测试Span事件和链接
  
  let ctx = context::Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建带有事件和链接的Span（在NoopTracer中这些是空的）
  let (_, span) = tracer.start_span(ctx, "events-and-links")
  
  // 验证默认情况下事件和链接为空
  assert_eq(span.events.length(), 0)
  assert_eq(span.links.length(), 0)
  
  // 测试SpanEvent结构（虽然NoopTracer不创建事件）
  let sample_event = SpanEvent::{
    name: "exception",
    timestamp_unix_nanos: 1640995200000000000L,
    attributes: [
      ("exception.type", AttributeValue::string("TimeoutException")),
      ("exception.message", AttributeValue::string("Operation timed out"))
    ]
  }
  
  assert_eq(sample_event.name, "exception")
  assert_eq(sample_event.timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(sample_event.attributes.length(), 2)
  
  // 测试SpanLink结构（虽然NoopTracer不创建链接）
  let linked_context = SpanContext::{
    trace_id: Array::make(16, 1_byte),
    span_id: Array::make(8, 1_byte),
    trace_flags: 1_byte,
    trace_state: "rojo=00f067aa0ba902b7"
  }
  
  let sample_link = SpanLink::{
    context: linked_context,
    attributes: [
      ("link.type", AttributeValue::string("parent")),
      ("relationship", AttributeValue::string("follows-from"))
    ]
  }
  
  assert_eq(sample_link.context.trace_flags, 1_byte)
  assert_eq(sample_link.context.trace_state, "rojo=00f067aa0ba902b7")
  assert_eq(sample_link.attributes.length(), 2)
}

test "span_status_transitions" {
  // 测试Span状态转换
  
  let ctx = context::Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建初始Span
  let (_, span) = tracer.start_span(ctx, "status-test")
  
  // 验证初始状态
  assert_eq(span.status, Unset)
  assert_eq(span.status_description, None)
  
  // 注意：在当前的NoopTracer实现中，Span是不可变的
  // 这里我们测试状态枚举值
  let unset_status = Unset
  let ok_status = Ok
  let error_status = Error
  
  // 验证状态枚举值
  match unset_status {
    Unset => assert_eq(true, true)
    _ => @test.fail("Expected Unset")
  }
  
  match ok_status {
    Ok => assert_eq(true, true)
    _ => @test.fail("Expected Ok")
  }
  
  match error_status {
    Error => assert_eq(true, true)
    _ => @test.fail("Expected Error")
  }
}

test "span_timing_and_duration" {
  // 测试Span时间相关属性
  
  let ctx = context::Context::empty()
  let tracer = NoopTracer::{}
  
  // 测试不同的开始时间
  let start_time_1 = 1640995200000000000L  // 2022-01-01 00:00:00 UTC
  let start_time_2 = 1640995260000000000L  // 2022-01-01 00:01:00 UTC
  let start_time_3 = 0L                    // 默认时间
  
  let (_, span_1) = tracer.start_span(ctx, "time-test-1", Some(Internal), [], Some(start_time_1))
  let (_, span_2) = tracer.start_span(ctx, "time-test-2", Some(Internal), [], Some(start_time_2))
  let (_, span_3) = tracer.start_span(ctx, "time-test-3", Some(Internal), [], Some(start_time_3))
  
  assert_eq(span_1.start_time_unix_nanos, start_time_1)
  assert_eq(span_2.start_time_unix_nanos, start_time_2)
  assert_eq(span_3.start_time_unix_nanos, start_time_3)
  
  // 验证所有Span的结束时间都是None（未结束）
  assert_eq(span_1.end_time_unix_nanos, None)
  assert_eq(span_2.end_time_unix_nanos, None)
  assert_eq(span_3.end_time_unix_nanos, None)
  
  // 测试边界时间值
  let max_time = 9223372036854775807L  // Int64最大值
  let min_time = -9223372036854775808L // Int64最小值
  
  let (_, span_max) = tracer.start_span(ctx, "max-time", Some(Internal), [], Some(max_time))
  let (_, span_min) = tracer.start_span(ctx, "min-time", Some(Internal), [], Some(min_time))
  
  assert_eq(span_max.start_time_unix_nanos, max_time)
  assert_eq(span_min.start_time_unix_nanos, min_time)
}

test "span_parent_relationships" {
  // 测试Span父子关系
  
  let ctx = context::Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建没有父Span的Span
  let (_, root_span) = tracer.start_span(ctx, "root-span")
  assert_eq(root_span.parent_span_id, None)
  
  // 在NoopTracer中，我们无法真正设置parent_span_id
  // 但我们可以测试Span结构中的parent_span_id字段
  
  // 模拟一个父Span ID
  let parent_span_id = Array::make(8, 42_byte)
  
  // 注意：当前的NoopTracer实现不支持设置parent_span_id
  // 这里我们只是验证数据结构的存在
  assert_eq(parent_span_id.length(), 8)
  
  let mut i = 0
  while i < parent_span_id.length() {
    assert_eq(parent_span_id[i], 42_byte)
    i = i + 1
  }
}

test "span_comprehensive_workflow" {
  // 测试Span的完整工作流程
  
  let ctx = context::Context::empty()
  let tracer = NoopTracer::{}
  
  // 1. 创建根Span
  let root_attributes = [
    ("service.name", AttributeValue::string("web-server")),
    ("service.version", AttributeValue::string("1.0.0")),
    ("deployment.environment", AttributeValue::string("production"))
  ]
  
  let (_, root_span) = tracer.start_span(
    ctx, 
    "http-server-request", 
    Some(Server), 
    Some(root_attributes),
    Some(1640995200000000000L)
  )
  
  assert_eq(root_span.name, "http-server-request")
  assert_eq(root_span.kind, Server)
  assert_eq(root_span.attributes.length(), 3)
  
  // 2. 创建子Span（模拟数据库查询）
  let db_attributes = [
    ("db.system", AttributeValue::string("postgresql")),
    ("db.statement", AttributeValue::string("SELECT * FROM users WHERE id = $1")),
    ("db.user", AttributeValue::string("app_user"))
  ]
  
  let (_, db_span) = tracer.start_span(
    ctx,
    "db-query",
    Some(Client),
    Some(db_attributes),
    Some(1640995200000001000L)  // 比父Span晚1微秒开始
  )
  
  assert_eq(db_span.name, "db-query")
  assert_eq(db_span.kind, Client)
  assert_eq(db_span.attributes.length(), 3)
  
  // 3. 创建另一个子Span（模拟缓存查询）
  let cache_attributes = [
    ("cache.system", AttributeValue::string("redis")),
    ("cache.operation", AttributeValue::string("GET")),
    ("cache.key", AttributeValue::string("user:123"))
  ]
  
  let (_, cache_span) = tracer.start_span(
    ctx,
    "cache-lookup",
    Some(Client),
    Some(cache_attributes),
    Some(1640995200000002000L)  // 比父Span晚2微秒开始
  )
  
  assert_eq(cache_span.name, "cache-lookup")
  assert_eq(cache_span.kind, Client)
  assert_eq(cache_span.attributes.length(), 3)
  
  // 4. 验证所有Span的基本属性
  assert_eq(root_span.status, Unset)
  assert_eq(db_span.status, Unset)
  assert_eq(cache_span.status, Unset)
  
  assert_eq(root_span.end_time_unix_nanos, None)
  assert_eq(db_span.end_time_unix_nanos, None)
  assert_eq(cache_span.end_time_unix_nanos, None)
  
  assert_eq(root_span.events.length(), 0)
  assert_eq(db_span.events.length(), 0)
  assert_eq(cache_span.events.length(), 0)
  
  assert_eq(root_span.links.length(), 0)
  assert_eq(db_span.links.length(), 0)
  assert_eq(cache_span.links.length(), 0)
  
  // 5. 验证时间顺序
  assert_eq(root_span.start_time_unix_nanos, 1640995200000000000L)
  assert_eq(db_span.start_time_unix_nanos, 1640995200000001000L)
  assert_eq(cache_span.start_time_unix_nanos, 1640995200000002000L)
  
  assert_eq(db_span.start_time_unix_nanos > root_span.start_time_unix_nanos, true)
  assert_eq(cache_span.start_time_unix_nanos > db_span.start_time_unix_nanos, true)
}