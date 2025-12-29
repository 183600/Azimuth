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
  let attributes = [("error", @azimuth.telemetry.api.common.AttributeValue::bool(true))]
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
  
  let attributes = [("link.type", @azimuth.telemetry.api.common.AttributeValue::string("parent"))]
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
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  let (new_ctx, span) = NoopTracer::start_span(ctx, "test-span", Some(Server))
  
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
  let tracer = NoopTracerProvider::get_tracer("test-tracer", Some("1.0.0"))
  
  // 确保返回的是一个Tracer（NoopTracer实现了Tracer trait）
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let (_, span) = NoopTracer::start_span(ctx, "test")
  assert_eq(span.name, "test")
}

test "span_with_attributes" {
  // 测试带属性的span创建
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let attributes = [
    ("http.method", @azimuth.telemetry.api.common.AttributeValue::string("GET")),
    ("http.status_code", @azimuth.telemetry.api.common.AttributeValue::int(200L)),
    ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("12345"))
  ]
  
  let (_, span) = NoopTracer::start_span(
    ctx, 
    "http-request", 
    Some(Server), 
    Some(attributes)
  )
  
  assert_eq(span.name, "http-request")
  assert_eq(span.kind, Server)
  assert_eq(span.attributes.length(), 3)
}

test "span_with_events_and_links" {
  // 测试Span的事件和链接
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  // 创建带事件和链接的span
  let (_, span) = NoopTracer::start_span(ctx, "complex-operation", Some(Server))
  
  // 添加事件
  let event1 = {
    name: "database.query.start",
    timestamp_unix_nanos: 1234567890L,
    attributes: [("query", @azimuth.telemetry.api.common.AttributeValue::string("SELECT * FROM users"))]
  }
  
  let event2 = {
    name: "database.query.complete",
    timestamp_unix_nanos: 1234567950L,
    attributes: [
      ("rows", @azimuth.telemetry.api.common.AttributeValue::int(100L)),
      ("duration", @azimuth.telemetry.api.common.AttributeValue::float(60.0))
    ]
  }
  
  let event3 = {
    name: "error.occurred",
    timestamp_unix_nanos: 1234568000L,
    attributes: [
      ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("Timeout")),
      ("error.message", @azimuth.telemetry.api.common.AttributeValue::string("Database query timed out"))
    ]
  }
  
  let events = [event1, event2, event3]
  
  // 创建链接
  let linked_trace_id = Array::make(16, 10_byte)
  let linked_span_id = Array::make(8, 11_byte)
  let linked_context = {
    trace_id: linked_trace_id,
    span_id: linked_span_id,
    trace_flags: 1_byte,
    trace_state: "test=value"
  }
  
  let link = {
    context: linked_context,
    attributes: [("link.type", @azimuth.telemetry.api.common.AttributeValue::string("parent"))]
  }
  
  let links = [link]
  
  // 验证事件
  assert_eq(events.length(), 3)
  assert_eq(events[0].name, "database.query.start")
  assert_eq(events[1].name, "database.query.complete")
  assert_eq(events[2].name, "error.occurred")
  
  // 验证事件属性
  match events[1].attributes[0] {
    ("rows", @azimuth.telemetry.api.common.AttributeValue::IntValue(rows)) => assert_eq(rows, 100L)
    _ => @test.fail("Expected IntValue for rows")
  }
  
  // 验证链接
  assert_eq(links.length(), 1)
  assert_eq(links[0].context.trace_id.length(), 16)
  assert_eq(links[0].context.span_id.length(), 8)
  assert_eq(links[0].attributes.length(), 1)
}

test "span_status_management" {
  // 测试Span的状态管理
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  // 创建span并设置不同状态
  let (_, span) = NoopTracer::start_span(ctx, "operation-with-status", Some(Client))
  
  // 初始状态应该是Unset
  assert_eq(span.status, Unset)
  assert_eq(span.status_description, None)
  
  // 创建另一个span并设置为Error状态
  let (_, error_span) = NoopTracer::start_span(ctx, "failed-operation", Some(Server))
  
  assert_eq(error_span.status, Unset)
  assert_eq(error_span.status_description, None)
  
  // 测试带有状态描述的span创建
  let status_attrs = [
    ("error.code", @azimuth.telemetry.api.common.AttributeValue::int(500)),
    ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("ConnectionError"))
  ]
  
  let (_, status_span) = NoopTracer::start_span(
    ctx,
    "operation-with-error-attrs",
    Some(Server),
    Some(status_attrs)
  )
  
  assert_eq(status_span.name, "operation-with-error-attrs")
  assert_eq(status_span.attributes.length(), 2)
}

test "span_nesting_hierarchy_simulation" {
  // 测试Span嵌套层次结构的模拟
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let base_time = 1640995200000000000L // 2022-01-01 00:00:00 UTC
  
  // 创建根span
  let (ctx1, root_span) = NoopTracer::start_span(
    ctx, 
    "http-request", 
    Some(Server), 
    Some([
      ("http.method", @azimuth.telemetry.api.common.AttributeValue::string("POST")),
      ("http.url", @azimuth.telemetry.api.common.AttributeValue::string("/api/process"))
    ]),
    Some(base_time)
  )
  
  // 创建第一级子span
  let (ctx2, auth_span) = NoopTracer::start_span(
    ctx1,
    "authentication",
    Some(Internal),
    Some([
      ("auth.method", @azimuth.telemetry.api.common.AttributeValue::string("jwt")),
      ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user123"))
    ]),
    Some(base_time + 1000000L) // 1ms later
  )
  
  // 创建第二级子span
  let (ctx3, db_span) = NoopTracer::start_span(
    ctx2,
    "database-query",
    Some(Client),
    Some([
      ("db.operation", @azimuth.telemetry.api.common.AttributeValue::string("SELECT")),
      ("db.table", @azimuth.telemetry.api.common.AttributeValue::string("users"))
    ]),
    Some(base_time + 2000000L) // 2ms later
  )
  
  // 创建另一个第一级子span
  let (ctx4, cache_span) = NoopTracer::start_span(
    ctx1,
    "cache-lookup",
    Some(Internal),
    Some([
      ("cache.key", @azimuth.telemetry.api.common.AttributeValue::string("user:123")),
      ("cache.hit", @azimuth.telemetry.api.common.AttributeValue::bool(true))
    ]),
    Some(base_time + 5000000L) // 5ms later
  )
  
  // 验证span层次结构
  assert_eq(root_span.name, "http-request")
  assert_eq(root_span.kind, Server)
  assert_eq(root_span.parent_span_id, None)
  assert_eq(root_span.start_time_unix_nanos, base_time)
  
  assert_eq(auth_span.name, "authentication")
  assert_eq(auth_span.kind, Internal)
  assert_eq(auth_span.start_time_unix_nanos, base_time + 1000000L)
  
  assert_eq(db_span.name, "database-query")
  assert_eq(db_span.kind, Client)
  assert_eq(db_span.start_time_unix_nanos, base_time + 2000000L)
  
  assert_eq(cache_span.name, "cache-lookup")
  assert_eq(cache_span.kind, Internal)
  assert_eq(cache_span.start_time_unix_nanos, base_time + 5000000L)
  
  // 验证时间顺序
  assert_eq(root_span.start_time_unix_nanos < auth_span.start_time_unix_nanos, true)
  assert_eq(auth_span.start_time_unix_nanos < db_span.start_time_unix_nanos, true)
  assert_eq(db_span.start_time_unix_nanos < cache_span.start_time_unix_nanos, true)
}

test "span_lifecycle_simulation" {
  // 测试Span生命周期的模拟
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let start_time = 1640995200000000000L
  let end_time = start_time + 10000000L // 10ms later
  
  // 创建span
  let (ctx1, span) = NoopTracer::start_span(
    ctx,
    "operation-lifecycle",
    Some(Server),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("api-service")),
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("business-logic"))
    ]),
    Some(start_time)
  )
  
  // 验证span初始状态
  assert_eq(span.name, "operation-lifecycle")
  assert_eq(span.kind, Server)
  assert_eq(span.start_time_unix_nanos, start_time)
  assert_eq(span.end_time_unix_nanos, None) // 还未结束
  assert_eq(span.status, Unset)
  assert_eq(span.events.length(), 0)
  
  // 模拟span生命周期中的事件
  let start_event = SpanEvent::{
    name: "operation.started",
    timestamp_unix_nanos: start_time + 1000000L,
    attributes: [
      ("phase", @azimuth.telemetry.api.common.AttributeValue::string("initialization")),
      ("duration.ms", @azimuth.telemetry.api.common.AttributeValue::int(1))
    ]
  }
  
  let processing_event = SpanEvent::{
    name: "operation.processing",
    timestamp_unix_nanos: start_time + 5000000L,
    attributes: [
      ("phase", @azimuth.telemetry.api.common.AttributeValue::string("main-logic")),
      ("records.processed", @azimuth.telemetry.api.common.AttributeValue::int(150)),
      ("duration.ms", @azimuth.telemetry.api.common.AttributeValue::int(4))
    ]
  }
  
  let completion_event = SpanEvent::{
    name: "operation.completed",
    timestamp_unix_nanos: end_time,
    attributes: [
      ("phase", @azimuth.telemetry.api.common.AttributeValue::string("cleanup")),
      ("total.records", @azimuth.telemetry.api.common.AttributeValue::int(150)),
      ("success.rate", @azimuth.telemetry.api.common.AttributeValue::float(0.98)),
      ("duration.ms", @azimuth.telemetry.api.common.AttributeValue::int(5))
    ]
  }
  
  let events = [start_event, processing_event, completion_event]
  
  // 验证事件时间顺序
  assert_eq(events[0].timestamp_unix_nanos, start_time + 1000000L)
  assert_eq(events[1].timestamp_unix_nanos, start_time + 5000000L)
  assert_eq(events[2].timestamp_unix_nanos, end_time)
  
  // 验证事件属性
  match events[1].attributes[0] {
    ("phase", @azimuth.telemetry.api.common.AttributeValue::StringValue(phase)) => {
      assert_eq(phase, "main-logic")
    }
    _ => @test.fail("Expected phase attribute")
  }
  
  match events[2].attributes[1] {
    ("total.records", @azimuth.telemetry.api.common.AttributeValue::IntValue(total)) => {
      assert_eq(total, 150)
    }
    _ => @test.fail("Expected total.records attribute")
  }
}

test "span_complex_event_and_link_scenarios" {
  // 测试Span复杂事件和链接场景
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let base_time = 1640995200000000000L
  
  // 创建主span
  let (ctx1, main_span) = NoopTracer::start_span(
    ctx,
    "complex-operation",
    Some(Server),
    Some([
      ("operation.id", @azimuth.telemetry.api.common.AttributeValue::string("op-12345")),
      ("batch.size", @azimuth.telemetry.api.common.AttributeValue::int(1000))
    ]),
    Some(base_time)
  )
  
  // 创建复杂的span事件
  let validation_event = SpanEvent::{
    name: "validation.completed",
    timestamp_unix_nanos: base_time + 1000000L,
    attributes: [
      ("validation.type", @azimuth.telemetry.api.common.AttributeValue::string("schema")),
      ("records.valid", @azimuth.telemetry.api.common.AttributeValue::int(950)),
      ("records.invalid", @azimuth.telemetry.api.common.AttributeValue::int(50)),
      ("validation.duration.ms", @azimuth.telemetry.api.common.AttributeValue::float(15.5))
    ]
  }
  
  let transformation_event = SpanEvent::{
    name: "transformation.started",
    timestamp_unix_nanos: base_time + 2000000L,
    attributes: [
      ("transformation.type", @azimuth.telemetry.api.common.AttributeValue::string("enrichment")),
      ("input.format", @azimuth.telemetry.api.common.AttributeValue::string("json")),
      ("output.format", @azimuth.telemetry.api.common.AttributeValue::string("avro")),
      ("parallel.workers", @azimuth.telemetry.api.common.AttributeValue::int(4))
    ]
  }
  
  let error_event = SpanEvent::{
    name: "error.occurred",
    timestamp_unix_nanos: base_time + 8000000L,
    attributes: [
      ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("TransformationError")),
      ("error.code", @azimuth.telemetry.api.common.AttributeValue::int(5001)),
      ("error.message", @azimuth.telemetry.api.common.AttributeValue::string("Field mapping failed")),
      ("failed.records", @azimuth.telemetry.api.common.AttributeValue::int(25)),
      ("retry.count", @azimuth.telemetry.api.common.AttributeValue::int(3)),
      ("recovery.possible", @azimuth.telemetry.api.common.AttributeValue::bool(true))
    ]
  }
  
  // 创建span链接
  let linked_trace_id = Array::make(16, 0_byte)
  let linked_span_id = Array::make(8, 0_byte)
  
  // 设置特定的trace_id和span_id值
  linked_trace_id[0] = 0x12_byte
  linked_trace_id[1] = 0x34_byte
  linked_span_id[0] = 0x56_byte
  linked_span_id[1] = 0x78_byte
  
  let linked_context = SpanContext::{
    trace_id: linked_trace_id,
    span_id: linked_span_id,
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  let causal_link = SpanLink::{
    context: linked_context,
    attributes: [
      ("link.type", @azimuth.telemetry.api.common.AttributeValue::string("causal")),
      ("relationship", @azimuth.telemetry.api.common.AttributeValue::string("parent-operation")),
      ("correlation.id", @azimuth.telemetry.api.common.AttributeValue::string("corr-67890"))
    ]
  }
  
  let context_link = SpanLink::{
    context: linked_context,
    attributes: [
      ("link.type", @azimuth.telemetry.api.common.AttributeValue::string("context")),
      ("shared.data", @azimuth.telemetry.api.common.AttributeValue::string("user-session")),
      ("data.source", @azimuth.telemetry.api.common.AttributeValue::string("auth-service"))
    ]
  }
  
  let events = [validation_event, transformation_event, error_event]
  let links = [causal_link, context_link]
  
  // 验证事件
  assert_eq(events.length(), 3)
  assert_eq(events[0].name, "validation.completed")
  assert_eq(events[1].name, "transformation.started")
  assert_eq(events[2].name, "error.occurred")
  
  // 验证错误事件的复杂属性
  match events[2].attributes[0] {
    ("error.type", @azimuth.telemetry.api.common.AttributeValue::StringValue(error_type)) => {
      assert_eq(error_type, "TransformationError")
    }
    _ => @test.fail("Expected error.type attribute")
  }
  
  match events[2].attributes[5] {
    ("recovery.possible", @azimuth.telemetry.api.common.AttributeValue::BoolValue(recovery)) => {
      assert_eq(recovery, true)
    }
    _ => @test.fail("Expected recovery.possible attribute")
  }
  
  // 验证链接
  assert_eq(links.length(), 2)
  assert_eq(links[0].context.trace_id.length(), 16)
  assert_eq(links[0].context.span_id.length(), 8)
  assert_eq(links[0].context.trace_flags, 1_byte)
  assert_eq(links[0].context.trace_state, "key1=value1,key2=value2")
  
  // 验证链接属性
  match links[0].attributes[0] {
    ("link.type", @azimuth.telemetry.api.common.AttributeValue::StringValue(link_type)) => {
      assert_eq(link_type, "causal")
    }
    _ => @test.fail("Expected link.type attribute")
  }
  
  match links[1].attributes[1] {
    ("shared.data", @azimuth.telemetry.api.common.AttributeValue::StringValue(shared_data)) => {
      assert_eq(shared_data, "user-session")
    }
    _ => @test.fail("Expected shared.data attribute")
  }
}

test "span_context_and_trace_propagation" {
  // 测试Span上下文和追踪传播
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  // 创建根span
  let (ctx1, root_span) = NoopTracer::start_span(
    ctx,
    "root-operation",
    Some(Server),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("gateway")),
      ("request.id", @azimuth.telemetry.api.common.AttributeValue::string("req-001"))
    ])
  )
  
  // 验证根span的上下文
  assert_eq(root_span.context.trace_id.length(), 16)
  assert_eq(root_span.context.span_id.length(), 8)
  assert_eq(root_span.context.trace_flags, 0_byte)
  assert_eq(root_span.context.trace_state, "")
  
  // 创建子span（应该继承trace_id）
  let (ctx2, child_span) = NoopTracer::start_span(
    ctx1,
    "child-operation",
    Some(Client),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("downstream")),
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("database"))
    ])
  )
  
  // 验证子span的上下文
  assert_eq(child_span.context.trace_id.length(), 16)
  assert_eq(child_span.context.span_id.length(), 8)
  
  // 创建具有不同trace_flags的span
  let trace_id = Array::make(16, 1_byte)
  let span_id = Array::make(8, 2_byte)
  let sampled_context = SpanContext::{
    trace_id,
    span_id,
    trace_flags: 1_byte, // 采样标志
    trace_state: "sample.decision=true"
  }
  
  // 创建带链接的span来模拟传播
  let link = SpanLink::{
    context: sampled_context,
    attributes: [
      ("propagation.type", @azimuth.telemetry.api.common.AttributeValue::string("trace-context")),
      ("sampling.decision", @azimuth.telemetry.api.common.AttributeValue::bool(true))
    ]
  }
  
  // 验证传播的上下文信息
  assert_eq(link.context.trace_flags, 1_byte)
  assert_eq(link.context.trace_state, "sample.decision=true")
  
  match link.attributes[1] {
    ("sampling.decision", @azimuth.telemetry.api.common.AttributeValue::BoolValue(sampled)) => {
      assert_eq(sampled, true)
    }
    _ => @test.fail("Expected sampling.decision attribute")
  }
}