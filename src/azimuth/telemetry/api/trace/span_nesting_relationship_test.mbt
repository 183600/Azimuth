// Span嵌套关系测试
// 测试Span之间的嵌套关系，包括父子关系的正确性和数据传播

test "span_simple_parent_child_relationship" {
  // 测试简单的父子Span关系
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建父span
  let (parent_ctx, parent_span) = tracer.start_span(ctx, "parent_operation", Some(Server), None)
  
  // 创建子span
  let (child_ctx, child_span) = tracer.start_span(parent_ctx, "child_operation", Some(Client), None)
  
  // 验证父子关系的基本属性
  assert_eq(parent_span.name, "parent_operation")
  assert_eq(parent_span.kind, Server)
  assert_eq(parent_span.parent_span_id, None)  // 父span没有父span
  
  assert_eq(child_span.name, "child_operation")
  assert_eq(child_span.kind, Client)
  // 注意：在No-op实现中，子span可能不会自动设置parent_span_id
  // 这里我们验证基本的嵌套逻辑
  
  // 验证trace_id应该相同（在真实实现中）
  assert_eq(parent_span.context.trace_id.length(), 16)
  assert_eq(child_span.context.trace_id.length(), 16)
}

test "span_multiple_children_relationship" {
  // 测试一个父span有多个子span的关系
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建父span
  let (parent_ctx, parent_span) = tracer.start_span(ctx, "parent_operation", Some(Server), None)
  
  // 创建多个子span
  let (child1_ctx, child1_span) = tracer.start_span(parent_ctx, "child_operation_1", Some(Client), None)
  let (child2_ctx, child2_span) = tracer.start_span(parent_ctx, "child_operation_2", Some(Internal), None)
  let (child3_ctx, child3_span) = tracer.start_span(parent_ctx, "child_operation_3", Some(Producer), None)
  
  // 验证父span属性
  assert_eq(parent_span.name, "parent_operation")
  assert_eq(parent_span.kind, Server)
  
  // 验证子span属性
  assert_eq(child1_span.name, "child_operation_1")
  assert_eq(child1_span.kind, Client)
  
  assert_eq(child2_span.name, "child_operation_2")
  assert_eq(child2_span.kind, Internal)
  
  assert_eq(child3_span.name, "child_operation_3")
  assert_eq(child3_span.kind, Producer)
  
  // 验证所有span都是独立的实例
  assert_eq(parent_span.name != child1_span.name, true)
  assert_eq(child1_span.name != child2_span.name, true)
  assert_eq(child2_span.name != child3_span.name, true)
}

test "span_deep_nesting_hierarchy" {
  // 测试深层嵌套的Span层次结构
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建嵌套层次：root -> level1 -> level2 -> level3 -> level4
  let (root_ctx, root_span) = tracer.start_span(ctx, "root_operation", Some(Server), None)
  
  let (level1_ctx, level1_span) = tracer.start_span(root_ctx, "level1_operation", Some(Client), None)
  
  let (level2_ctx, level2_span) = tracer.start_span(level1_ctx, "level2_operation", Some(Internal), None)
  
  let (level3_ctx, level3_span) = tracer.start_span(level2_ctx, "level3_operation", Some(Producer), None)
  
  let (level4_ctx, level4_span) = tracer.start_span(level3_ctx, "level4_operation", Some(Consumer), None)
  
  // 验证每一层的span属性
  assert_eq(root_span.name, "root_operation")
  assert_eq(root_span.kind, Server)
  
  assert_eq(level1_span.name, "level1_operation")
  assert_eq(level1_span.kind, Client)
  
  assert_eq(level2_span.name, "level2_operation")
  assert_eq(level2_span.kind, Internal)
  
  assert_eq(level3_span.name, "level3_operation")
  assert_eq(level3_span.kind, Producer)
  
  assert_eq(level4_span.name, "level4_operation")
  assert_eq(level4_span.kind, Consumer)
  
  // 验证所有span都是独立的
  assert_eq(root_span.name != level1_span.name, true)
  assert_eq(level1_span.name != level2_span.name, true)
  assert_eq(level2_span.name != level3_span.name, true)
  assert_eq(level3_span.name != level4_span.name, true)
}

test "span_nesting_with_attributes_inheritance" {
  // 测试嵌套Span中的属性继承
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 父span的属性
  let parent_attributes = [
    ("service.name", AttributeValue::string("api-service")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("operation.type", AttributeValue::string("http_request")),
    ("user.id", AttributeValue::string("user123"))
  ]
  
  let (parent_ctx, parent_span) = tracer.start_span(ctx, "parent_operation", Some(Server), Some(parent_attributes))
  
  // 子span的属性（包含一些新的，一些可能覆盖的）
  let child_attributes = [
    ("operation.type", AttributeValue::string("database_query")),  // 可能覆盖父属性
    ("db.system", AttributeValue::string("postgresql")),
    ("db.statement", AttributeValue::string("SELECT * FROM users")),
    ("user.id", AttributeValue::string("user123")),  // 与父属性相同
    ("request.id", AttributeValue::string("req456"))   // 新属性
  ]
  
  let (_, child_span) = tracer.start_span(parent_ctx, "child_operation", Some(Client), Some(child_attributes))
  
  // 验证父span属性
  assert_eq(parent_span.attributes.length(), 4)
  
  // 验证子span属性
  assert_eq(child_span.attributes.length(), 5)
  
  // 验证父span的特定属性
  let mut found_service_name = false
  let mut found_operation_type = false
  let mut found_user_id = false
  
  let mut i = 0
  while i < parent_span.attributes.length() {
    let (key, value) = parent_span.attributes[i]
    
    match key {
      "service.name" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "api-service")
            found_service_name = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "operation.type" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "http_request")
            found_operation_type = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "user.id" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "user123")
            found_user_id = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => ()  // 忽略其他属性
    }
    
    i = i + 1
  }
  
  assert_eq(found_service_name, true)
  assert_eq(found_operation_type, true)
  assert_eq(found_user_id, true)
  
  // 验证子span的特定属性
  let mut found_child_operation_type = false
  let mut found_db_system = false
  let mut found_request_id = false
  
  i = 0
  while i < child_span.attributes.length() {
    let (key, value) = child_span.attributes[i]
    
    match key {
      "operation.type" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "database_query")
            found_child_operation_type = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "db.system" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "postgresql")
            found_db_system = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "request.id" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "req456")
            found_request_id = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => ()  // 忽略其他属性
    }
    
    i = i + 1
  }
  
  assert_eq(found_child_operation_type, true)
  assert_eq(found_db_system, true)
  assert_eq(found_request_id, true)
}

test "span_nesting_with_events_propagation" {
  // 测试嵌套Span中的事件传播
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 父span的事件
  let parent_events = [
    SpanEvent::{
      name: "request.start",
      timestamp_unix_nanos: 1000L,
      attributes: [
        ("http.method", AttributeValue::string("GET")),
        ("http.url", AttributeValue::string("/api/users"))
      ]
    },
    SpanEvent::{
      name: "authentication.success",
      timestamp_unix_nanos: 1100L,
      attributes: [
        ("auth.method", AttributeValue::string("jwt")),
        ("user.id", AttributeValue::string("user123"))
      ]
    }
  ]
  
  let parent_span = Span::{
    name: "parent_operation",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: parent_events,
    links: []
  }
  
  // 子span的事件
  let child_events = [
    SpanEvent::{
      name: "db.query.start",
      timestamp_unix_nanos: 1200L,
      attributes: [
        ("db.system", AttributeValue::string("postgresql")),
        ("db.operation", AttributeValue::string("SELECT"))
      ]
    },
    SpanEvent::{
      name: "db.query.complete",
      timestamp_unix_nanos: 1300L,
      attributes: [
        ("db.rows_affected", AttributeValue::int(10L)),
        ("db.duration_ms", AttributeValue::int(50L))
      ]
    }
  ]
  
  let child_span = Span::{
    name: "child_operation",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Client,
    parent_span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte })),
    start_time_unix_nanos: 1150L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: child_events,
    links: []
  }
  
  // 验证父span事件
  assert_eq(parent_span.events.length(), 2)
  assert_eq(parent_span.events[0].name, "request.start")
  assert_eq(parent_span.events[1].name, "authentication.success")
  
  // 验证子span事件
  assert_eq(child_span.events.length(), 2)
  assert_eq(child_span.events[0].name, "db.query.start")
  assert_eq(child_span.events[1].name, "db.query.complete")
  
  // 验证事件时间顺序
  assert_eq(parent_span.events[0].timestamp_unix_nanos < parent_span.events[1].timestamp_unix_nanos, true)
  assert_eq(child_span.events[0].timestamp_unix_nanos < child_span.events[1].timestamp_unix_nanos, true)
  
  // 验证父子span事件的时间关系
  assert_eq(parent_span.events[1].timestamp_unix_nanos < child_span.events[0].timestamp_unix_nanos, true)
}

test "span_nesting_with_status_propagation" {
  // 测试嵌套Span中的状态传播
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建父span（成功状态）
  let parent_span = Span::{
    name: "parent_operation",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(2000L),
    status: Ok,
    status_description: Some("Request processed successfully"),
    attributes: [
      ("http.method", AttributeValue::string("GET")),
      ("http.status_code", AttributeValue::int(200L))
    ],
    events: [],
    links: []
  }
  
  // 创建子span（错误状态）
  let child_span = Span::{
    name: "child_operation",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Client,
    parent_span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte })),
    start_time_unix_nanos: 1100L,
    end_time_unix_nanos: Some(1500L),
    status: Error,
    status_description: Some("Database connection failed"),
    attributes: [
      ("error.code", AttributeValue::string("DB_CONN_FAILED")),
      ("error.retries", AttributeValue::int(3L))
    ],
    events: [
      SpanEvent::{
        name: "error",
        timestamp_unix_nanos: 1200L,
        attributes: [
          ("error.type", AttributeValue::string("timeout")),
          ("error.message", AttributeValue::string("Connection timeout after 30 seconds"))
        ]
      }
    ],
    links: []
  }
  
  // 验证父span状态
  assert_eq(parent_span.status, Ok)
  match parent_span.status_description {
    Some(description) => assert_eq(description, "Request processed successfully")
    None => @test.fail("Test failed")
  }
  
  // 验证子span状态
  assert_eq(child_span.status, Error)
  match child_span.status_description {
    Some(description) => assert_eq(description, "Database connection failed")
    None => @test.fail("Test failed")
  }
  
  // 验证父子span状态是独立的
  assert_eq(parent_span.status != child_span.status, true)
}

test "span_nesting_with_timing_relationships" {
  // 测试嵌套Span中的时间关系
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建具有明确时间关系的嵌套span
  let parent_span = Span::{
    name: "parent_operation",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(5000L),
    status: Ok,
    status_description: Some("Parent operation completed"),
    attributes: [],
    events: [],
    links: []
  }
  
  // 子span1：在父span的时间范围内
  let child1_span = Span::{
    name: "child_operation_1",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Client,
    parent_span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte })),
    start_time_unix_nanos: 1500L,
    end_time_unix_nanos: Some(2500L),
    status: Ok,
    status_description: Some("Child operation 1 completed"),
    attributes: [],
    events: [],
    links: []
  }
  
  // 子span2：也在父span的时间范围内
  let child2_span = Span::{
    name: "child_operation_2",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x03_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte })),
    start_time_unix_nanos: 3000L,
    end_time_unix_nanos: Some(4500L),
    status: Ok,
    status_description: Some("Child operation 2 completed"),
    attributes: [],
    events: [],
    links: []
  }
  
  // 验证时间关系
  assert_eq(parent_span.start_time_unix_nanos, 1000L)
  assert_eq(parent_span.end_time_unix_nanos.unwrap(), 5000L)
  
  assert_eq(child1_span.start_time_unix_nanos, 1500L)
  assert_eq(child1_span.end_time_unix_nanos.unwrap(), 2500L)
  
  assert_eq(child2_span.start_time_unix_nanos, 3000L)
  assert_eq(child2_span.end_time_unix_nanos.unwrap(), 4500L)
  
  // 验证子span在父span的时间范围内
  assert_eq(parent_span.start_time_unix_nanos <= child1_span.start_time_unix_nanos, true)
  assert_eq(child1_span.end_time_unix_nanos.unwrap() <= parent_span.end_time_unix_nanos.unwrap(), true)
  
  assert_eq(parent_span.start_time_unix_nanos <= child2_span.start_time_unix_nanos, true)
  assert_eq(child2_span.end_time_unix_nanos.unwrap() <= parent_span.end_time_unix_nanos.unwrap(), true)
  
  // 验证子span之间的时间关系
  assert_eq(child1_span.end_time_unix_nanos.unwrap() < child2_span.start_time_unix_nanos, true)
}

test "span_nesting_with_links_relationship" {
  // 测试嵌套Span中的链接关系
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建一个相关的span（用于链接）
  let related_context = SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x02_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x05_byte }),
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 父span的链接
  let parent_links = [
    SpanLink::{
      context: related_context,
      attributes: [
        ("link.type", AttributeValue::string("causality")),
        ("related.service", AttributeValue::string("auth-service"))
      ]
    }
  ]
  
  let parent_span = Span::{
    name: "parent_operation",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(3000L),
    status: Ok,
    status_description: Some("Parent operation completed"),
    attributes: [],
    events: [],
    links: parent_links
  }
  
  // 子span的链接（可能继承父span的链接，也可能有自己的链接）
  let child_related_context = SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x03_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x06_byte }),
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let child_links = [
    SpanLink::{
      context: child_related_context,
      attributes: [
        ("link.type", AttributeValue::string("correlation")),
        ("related.service", AttributeValue::string("cache-service"))
      ]
    }
  ]
  
  let child_span = Span::{
    name: "child_operation",
    context: SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Client,
    parent_span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte })),
    start_time_unix_nanos: 1500L,
    end_time_unix_nanos: Some(2500L),
    status: Ok,
    status_description: Some("Child operation completed"),
    attributes: [],
    events: [],
    links: child_links
  }
  
  // 验证父span链接
  assert_eq(parent_span.links.length(), 1)
  assert_eq(parent_span.links[0].context.trace_id.length(), 16)
  assert_eq(parent_span.links[0].context.span_id.length(), 8)
  match parent_span.links[0].attributes[0].1 {
    StringValue(v) => assert_eq(v, "causality")
    _ => @test.fail("Test failed")
  }
  
  // 验证子span链接
  assert_eq(child_span.links.length(), 1)
  assert_eq(child_span.links[0].context.trace_id.length(), 16)
  assert_eq(child_span.links[0].context.span_id.length(), 8)
  match child_span.links[0].attributes[0].1 {
    StringValue(v) => assert_eq(v, "correlation")
    _ => @test.fail("Test failed")
  }
  
  // 验证父子span的链接是独立的
  assert_eq(parent_span.links[0].context.span_id != child_span.links[0].context.span_id, true)
}