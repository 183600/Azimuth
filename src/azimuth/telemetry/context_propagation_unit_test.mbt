// 上下文传播和链路追踪功能测试
// 测试分布式追踪上下文传播、Span关系和追踪链管理

use azimuth.telemetry.api.common
use azimuth.telemetry.api.trace
use azimuth.telemetry.api.context

test "trace_context_generation_and_validation" {
  // 测试追踪上下文生成和验证
  
  // 生成有效的trace_id (16字节)
  let trace_id_bytes = [for i = 0; i < 16; i = i + 1].map(fn(i) { (i % 256).to_byte() })
  
  // 验证trace_id长度
  assert_eq(trace_id_bytes.length(), 16)
  
  // 验证trace_id不为全零（有效trace_id的规则）
  let mut has_non_zero = false
  let mut i = 0
  while i < trace_id_bytes.length() {
    if trace_id_bytes[i] != 0_byte {
      has_non_zero = true
    }
    i = i + 1
  }
  assert_eq(has_non_zero, true)
  
  // 生成有效的span_id (8字节)
  let span_id_bytes = [for i = 0; i < 8; i = i + 1].map(fn(i) { ((i + 1) % 256).to_byte() })
  
  // 验证span_id长度
  assert_eq(span_id_bytes.length(), 8)
  
  // 验证span_id不为全零
  let mut span_has_non_zero = false
  i = 0
  while i < span_id_bytes.length() {
    if span_id_bytes[i] != 0_byte {
      span_has_non_zero = true
    }
    i = i + 1
  }
  assert_eq(span_has_non_zero, true)
  
  // 创建SpanContext
  let span_context = trace::SpanContext::{
    trace_id: trace_id_bytes,
    span_id: span_id_bytes,
    trace_flags: 1_byte,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 验证SpanContext
  assert_eq(span_context.trace_id.length(), 16)
  assert_eq(span_context.span_id.length(), 8)
  assert_eq(span_context.trace_flags, 1_byte)
  assert_eq(span_context.trace_state, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
}

test "span_hierarchy_and_relationships" {
  // 测试Span层次结构和关系
  
  // 创建根Span
  let root_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })
  let root_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte })
  
  let root_span_context = trace::SpanContext::{
    trace_id: root_trace_id,
    span_id: root_span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let root_span = trace::Span::{
    name: "root-operation",
    context: root_span_context,
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200L,
    end_time_unix_nanos: Some(1640995300L),
    status: trace::Ok,
    status_description: Some("Root operation completed"),
    attributes: [("service.name", common::AttributeValue::string("api-gateway"))],
    events: [],
    links: []
  }
  
  // 验证根Span
  assert_eq(root_span.name, "root-operation")
  assert_eq(root_span.kind, trace::Server)
  assert_eq(root_span.parent_span_id, None)
  
  // 创建子Span
  let child_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 2_byte })
  
  let child_span_context = trace::SpanContext::{
    trace_id: root_trace_id, // 相同的trace_id
    span_id: child_span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let child_span = trace::Span::{
    name: "database-query",
    context: child_span_context,
    kind: trace::Client,
    parent_span_id: Some(root_span_id), // 父Span ID
    start_time_unix_nanos: 1640995210L,
    end_time_unix_nanos: Some(1640995250L),
    status: trace::Ok,
    status_description: Some("Database query completed"),
    attributes: [("db.system", common::AttributeValue::string("postgresql"))],
    events: [],
    links: []
  }
  
  // 验证子Span
  assert_eq(child_span.name, "database-query")
  assert_eq(child_span.kind, trace::Client)
  assert_eq(child_span.parent_span_id.unwrap(), root_span_id)
  assert_eq(child_span.context.trace_id, root_span.context.trace_id) // 相同的trace_id
  
  // 创建孙Span
  let grandchild_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 3_byte })
  
  let grandchild_span_context = trace::SpanContext::{
    trace_id: root_trace_id, // 相同的trace_id
    span_id: grandchild_span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let grandchild_span = trace::Span::{
    name: "cache-lookup",
    context: grandchild_span_context,
    kind: trace::Internal,
    parent_span_id: Some(child_span_id), // 父Span ID
    start_time_unix_nanos: 1640995220L,
    end_time_unix_nanos: Some(1640995230L),
    status: trace::Ok,
    status_description: Some("Cache lookup completed"),
    attributes: [("cache.hit", common::AttributeValue::bool(true))],
    events: [],
    links: []
  }
  
  // 验证孙Span
  assert_eq(grandchild_span.name, "cache-lookup")
  assert_eq(grandchild_span.kind, trace::Internal)
  assert_eq(grandchild_span.parent_span_id.unwrap(), child_span_id)
  assert_eq(grandchild_span.context.trace_id, root_span.context.trace_id) // 相同的trace_id
  
  // 验证Span层次关系
  assert_eq(root_span.context.span_id != child_span.context.span_id, true)
  assert_eq(child_span.context.span_id != grandchild_span.context.span_id, true)
  assert_eq(root_span.context.span_id != grandchild_span.context.span_id, true)
}

test "span_events_and_timing" {
  // 测试Span事件和时间
  
  let span_context = trace::SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte }),
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 创建带事件的Span
  let span_events = [
    trace::SpanEvent::{
      name: "database.connection.created",
      timestamp_unix_nanos: 1640995210L,
      attributes: [
        ("db.connection.id", common::AttributeValue::string("conn-123")),
        ("db.type", common::AttributeValue::string("postgresql"))
      ]
    },
    trace::SpanEvent::{
      name: "database.query.started",
      timestamp_unix_nanos: 1640995215L,
      attributes: [
        ("db.query.text", common::AttributeValue::string("SELECT * FROM users WHERE id = $1")),
        ("db.statement.type", common::AttributeValue::string("SELECT"))
      ]
    },
    trace::SpanEvent::{
      name: "database.query.completed",
      timestamp_unix_nanos: 1640995250L,
      attributes: [
        ("db.rows_affected", common::AttributeValue::int(1L)),
        ("db.execution_time_ms", common::AttributeValue::float(35.0))
      ]
    }
  ]
  
  let eventful_span = trace::Span::{
    name: "database-operation",
    context: span_context,
    kind: trace::Client,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200L,
    end_time_unix_nanos: Some(1640995300L),
    status: trace::Ok,
    status_description: Some("Database operation completed successfully"),
    attributes: [("service.name", common::AttributeValue::string("user-service"))],
    events: span_events,
    links: []
  }
  
  // 验证Span事件
  assert_eq(eventful_span.events.length(), 3)
  assert_eq(eventful_span.events[0].name, "database.connection.created")
  assert_eq(eventful_span.events[0].timestamp_unix_nanos, 1640995210L)
  assert_eq(eventful_span.events[0].attributes.length(), 2)
  assert_eq(eventful_span.events[0].attributes[0].0, "db.connection.id")
  assert_eq(eventful_span.events[0].attributes[0].1, common::AttributeValue::string("conn-123"))
  
  assert_eq(eventful_span.events[1].name, "database.query.started")
  assert_eq(eventful_span.events[1].timestamp_unix_nanos, 1640995215L)
  
  assert_eq(eventful_span.events[2].name, "database.query.completed")
  assert_eq(eventful_span.events[2].timestamp_unix_nanos, 1640995250L)
  assert_eq(eventful_span.events[2].attributes[1].1, common::AttributeValue::float(35.0))
  
  // 验证时间顺序
  assert_eq(eventful_span.events[0].timestamp_unix_nanos < eventful_span.events[1].timestamp_unix_nanos, true)
  assert_eq(eventful_span.events[1].timestamp_unix_nanos < eventful_span.events[2].timestamp_unix_nanos, true)
  
  // 验证Span时间范围
  assert_eq(eventful_span.start_time_unix_nanos, 1640995200L)
  assert_eq(eventful_span.end_time_unix_nanos.unwrap(), 1640995300L)
  assert_eq(eventful_span.start_time_unix_nanos < eventful_span.end_time_unix_nanos.unwrap(), true)
}

test "span_links_and_cross_trace_correlation" {
  // 测试Span链接和跨追踪关联
  
  let current_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })
  let current_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte })
  
  let current_span_context = trace::SpanContext::{
    trace_id: current_trace_id,
    span_id: current_span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 创建其他追踪的SpanContext用于链接
  let other_trace_id_1 = [for i = 0; i < 16; i = i + 1].map(fn(_) { 2_byte })
  let other_span_id_1 = [for i = 0; i < 8; i = i + 1].map(fn(_) { 2_byte })
  
  let other_context_1 = trace::SpanContext::{
    trace_id: other_trace_id_1,
    span_id: other_span_id_1,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let other_trace_id_2 = [for i = 0; i < 16; i = i + 1].map(fn(_) { 3_byte })
  let other_span_id_2 = [for i = 0; i < 8; i = i + 1].map(fn(_) { 3_byte })
  
  let other_context_2 = trace::SpanContext::{
    trace_id: other_trace_id_2,
    span_id: other_span_id_2,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 创建Span链接
  let span_links = [
    trace::SpanLink::{
      context: other_context_1,
      attributes: [
        ("link.type", common::AttributeValue::string("caused_by")),
        ("service.name", common::AttributeValue::string("payment-service"))
      ]
    },
    trace::SpanLink::{
      context: other_context_2,
      attributes: [
        ("link.type", common::AttributeValue::string("related_to")),
        ("service.name", common::AttributeValue::string("inventory-service"))
      ]
    }
  ]
  
  // 创建带链接的Span
  let linked_span = trace::Span::{
    name: "order-processing",
    context: current_span_context,
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200L,
    end_time_unix_nanos: Some(1640995300L),
    status: trace::Ok,
    status_description: Some("Order processed successfully"),
    attributes: [("service.name", common::AttributeValue::string("order-service"))],
    events: [],
    links: span_links
  }
  
  // 验证Span链接
  assert_eq(linked_span.links.length(), 2)
  assert_eq(linked_span.links[0].context.trace_id, other_trace_id_1)
  assert_eq(linked_span.links[0].context.span_id, other_span_id_1)
  assert_eq(linked_span.links[0].attributes[0].0, "link.type")
  assert_eq(linked_span.links[0].attributes[0].1, common::AttributeValue::string("caused_by"))
  
  assert_eq(linked_span.links[1].context.trace_id, other_trace_id_2)
  assert_eq(linked_span.links[1].context.span_id, other_span_id_2)
  assert_eq(linked_span.links[1].attributes[1].1, common::AttributeValue::string("inventory-service"))
  
  // 验证跨追踪关联
  assert_eq(linked_span.context.trace_id != linked_span.links[0].context.trace_id, true)
  assert_eq(linked_span.context.trace_id != linked_span.links[1].context.trace_id, true)
  assert_eq(linked_span.links[0].context.trace_id != linked_span.links[1].context.trace_id, true)
}

test "trace_flags_and_state_propagation" {
  // 测试追踪标志和状态传播
  
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte })
  
  // 测试不同的trace_flags
  let sampled_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 1_byte, // 采样标志
    trace_state: ""
  }
  
  let unsampled_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 0_byte, // 未采样
    trace_state: ""
  }
  
  // 验证采样标志
  assert_eq(sampled_context.trace_flags, 1_byte)
  assert_eq(unsampled_context.trace_flags, 0_byte)
  
  // 测试复杂的trace_state
  let complex_trace_state = "vendor1=value1,vendor2=value2,key3=val3,key4=val4"
  let complex_state_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 1_byte,
    trace_state: complex_trace_state
  }
  
  // 验证trace_state
  assert_eq(complex_state_context.trace_state, complex_trace_state)
  assert_eq(complex_state_context.trace_state.contains("vendor1=value1"), true)
  assert_eq(complex_state_context.trace_state.contains("vendor2=value2"), true)
  assert_eq(complex_state_context.trace_state.contains("key3=val3"), true)
  assert_eq(complex_state_context.trace_state.contains("key4=val4"), true)
  
  // 测试空trace_state
  let empty_state_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  assert_eq(empty_state_context.trace_state, "")
  assert_eq(empty_state_context.trace_state.length(), 0)
}

test "span_attributes_and_metadata" {
  // 测试Span属性和元数据
  
  let span_context = trace::SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte }),
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 创建丰富的Span属性
  let span_attributes = [
    ("service.name", common::AttributeValue::string("user-service")),
    ("service.version", common::AttributeValue::string("1.2.3")),
    ("service.instance.id", common::AttributeValue::string("instance-12345")),
    ("deployment.environment", common::AttributeValue::string("production")),
    ("http.method", common::AttributeValue::string("GET")),
    ("http.target", common::AttributeValue::string("/api/users/123")),
    ("http.status_code", common::AttributeValue::int(200L)),
    ("http.route", common::AttributeValue::string("/api/users/{id}")),
    ("user.id", common::AttributeValue::int(12345L)),
    ("user.role", common::AttributeValue::string("admin")),
    ("db.system", common::AttributeValue::string("postgresql")),
    ("db.statement", common::AttributeValue::string("SELECT * FROM users WHERE id = $1")),
    ("db.operation", common::AttributeValue::string("SELECT")),
    ("net.host.name", common::AttributeValue::string("api.example.com")),
    ("net.host.port", common::AttributeValue::int(443L)),
    ("net.protocol.version", common::AttributeValue::string("1.1")),
    ("process.pid", common::AttributeValue::int(1234L)),
    ("process.executable.name", common::AttributeValue::string("user-service")),
    ("process.runtime.name", common::AttributeValue::string("node")),
    ("process.runtime.version", common::AttributeValue::string("18.17.0")),
    ("cloud.provider", common::AttributeValue::string("aws")),
    ("cloud.region", common::AttributeValue::string("us-west-2")),
    ("cloud.availability_zone", common::AttributeValue::string("us-west-2a"))
  ]
  
  let rich_span = trace::Span::{
    name: "http-request",
    context: span_context,
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200L,
    end_time_unix_nanos: Some(1640995300L),
    status: trace::Ok,
    status_description: Some("HTTP request completed successfully"),
    attributes: span_attributes,
    events: [],
    links: []
  }
  
  // 验证Span属性
  assert_eq(rich_span.attributes.length(), 22)
  
  // 验证特定属性
  assert_eq(rich_span.attributes[0].0, "service.name")
  assert_eq(rich_span.attributes[0].1, common::AttributeValue::string("user-service"))
  
  assert_eq(rich_span.attributes[6].0, "http.status_code")
  assert_eq(rich_span.attributes[6].1, common::AttributeValue::int(200L))
  
  assert_eq(rich_span.attributes[9].0, "user.id")
  assert_eq(rich_span.attributes[9].1, common::AttributeValue::int(12345L))
  
  assert_eq(rich_span.attributes[21].0, "cloud.availability_zone")
  assert_eq(rich_span.attributes[21].1, common::AttributeValue::string("us-west-2a"))
  
  // 验证属性类型
  let mut string_attr_count = 0
  let mut int_attr_count = 0
  let mut i = 0
  while i < rich_span.attributes.length() {
    match rich_span.attributes[i].1 {
      StringValue(_) => string_attr_count = string_attr_count + 1
      IntValue(_) => int_attr_count = int_attr_count + 1
      _ => {}
    }
    i = i + 1
  }
  
  assert_eq(string_attr_count, 18)
  assert_eq(int_attr_count, 4)
}

test "span_status_and_error_handling" {
  // 测试Span状态和错误处理
  
  let span_context = trace::SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte }),
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 创建不同状态的Span
  let unset_span = trace::Span::{
    name: "unset-operation",
    context: span_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200L,
    end_time_unix_nanos: None,
    status: trace::Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  let ok_span = trace::Span::{
    name: "successful-operation",
    context: span_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200L,
    end_time_unix_nanos: Some(1640995300L),
    status: trace::Ok,
    status_description: Some("Operation completed successfully"),
    attributes: [],
    events: [],
    links: []
  }
  
  let error_span = trace::Span::{
    name: "failed-operation",
    context: span_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200L,
    end_time_unix_nanos: Some(1640995250L),
    status: trace::Error,
    status_description: Some("Database connection failed"),
    attributes: [
      ("error.type", common::AttributeValue::string("ConnectionError")),
      ("error.message", common::AttributeValue::string("Unable to connect to database")),
      ("error.stack_trace", common::AttributeValue::string("at Database.connect (db.js:123:45)"))
    ],
    events: [
      trace::SpanEvent::{
        name: "exception",
        timestamp_unix_nanos: 1640995245L,
        attributes: [
          ("exception.type", common::AttributeValue::string("ConnectionError")),
          ("exception.message", common::AttributeValue::string("ECONNREFUSED")),
          ("exception.stacktrace", common::AttributeValue::string("Error: connect ECONNREFUSED"))
        ]
      }
    ],
    links: []
  }
  
  // 验证不同状态的Span
  assert_eq(unset_span.status, trace::Unset)
  assert_eq(unset_span.status_description, None)
  assert_eq(unset_span.end_time_unix_nanos, None)
  
  assert_eq(ok_span.status, trace::Ok)
  assert_eq(ok_span.status_description.unwrap(), "Operation completed successfully")
  assert_eq(ok_span.end_time_unix_nanos.unwrap(), 1640995300L)
  
  assert_eq(error_span.status, trace::Error)
  assert_eq(error_span.status_description.unwrap(), "Database connection failed")
  assert_eq(error_span.end_time_unix_nanos.unwrap(), 1640995250L)
  
  // 验证错误Span的属性
  assert_eq(error_span.attributes.length(), 3)
  assert_eq(error_span.attributes[0].0, "error.type")
  assert_eq(error_span.attributes[0].1, common::AttributeValue::string("ConnectionError"))
  
  // 验证错误Span的事件
  assert_eq(error_span.events.length(), 1)
  assert_eq(error_span.events[0].name, "exception")
  assert_eq(error_span.events[0].attributes.length(), 3)
  assert_eq(error_span.events[0].attributes[0].0, "exception.type")
  assert_eq(error_span.events[0].attributes[0].1, common::AttributeValue::string("ConnectionError"))
}

test "concurrent_span_operations" {
  // 测试并发Span操作
  
  let base_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })
  let base_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte })
  
  let base_context = trace::SpanContext::{
    trace_id: base_trace_id,
    span_id: base_span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 创建并发操作的Span数组
  let concurrent_spans = [] : Array[trace::Span]
  let mut i = 0
  while i < 5 {
    let concurrent_span_id = [for j = 0; j < 8; j = j + 1].map(fn(j) { (i + 2 + j).to_byte() })
    
    let concurrent_context = trace::SpanContext::{
      trace_id: base_trace_id, // 相同的trace_id
      span_id: concurrent_span_id,
      trace_flags: 1_byte,
      trace_state: ""
    }
    
    let concurrent_span = trace::Span::{
      name: "concurrent-operation-" + i.to_string(),
      context: concurrent_context,
      kind: trace::Internal,
      parent_span_id: Some(base_span_id), // 相同的父Span
      start_time_unix_nanos: 1640995200L + i.to_int64(),
      end_time_unix_nanos: Some(1640995300L + i.to_int64()),
      status: trace::Ok,
      status_description: Some("Concurrent operation completed"),
      attributes: [
        ("operation.id", common::AttributeValue::int(i.to_int64())),
        ("thread.id", common::AttributeValue::string("thread-" + i.to_string()))
      ],
      events: [],
      links: []
    }
    
    concurrent_spans.push(concurrent_span)
    i = i + 1
  }
  
  // 验证并发Span
  assert_eq(concurrent_spans.length(), 5)
  
  // 验证所有Span都有相同的trace_id
  i = 0
  while i < concurrent_spans.length() {
    assert_eq(concurrent_spans[i].context.trace_id, base_trace_id)
    assert_eq(concurrent_spans[i].parent_span_id.unwrap(), base_span_id)
    i = i + 1
  }
  
  // 验证每个Span都有唯一的span_id
  i = 0
  while i < concurrent_spans.length() {
    let mut j = i + 1
    while j < concurrent_spans.length() {
      assert_eq(concurrent_spans[i].context.span_id != concurrent_spans[j].context.span_id, true)
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证Span名称和属性
  assert_eq(concurrent_spans[0].name, "concurrent-operation-0")
  assert_eq(concurrent_spans[4].name, "concurrent-operation-4")
  assert_eq(concurrent_spans[2].attributes[0].1, common::AttributeValue::int(2L))
  assert_eq(concurrent_spans[3].attributes[1].1, common::AttributeValue::string("thread-3"))
}