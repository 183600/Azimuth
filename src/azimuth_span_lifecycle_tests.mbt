// Azimuth Span Lifecycle Test Suite
// 测试跨度(Span)的完整生命周期，包括创建、设置、结束和状态管理

// 假设的Span相关类型定义（用于测试）
pub enum SpanKind {
  Server
  Client
  Producer
  Consumer
  Internal
}

pub enum StatusCode {
  Unset
  Ok
  Error
}

pub struct Span {
  name: String
  context: azimuth::SpanContext
  kind: SpanKind
  start_time: Int
  end_time: Option[Int]
  status: StatusCode
  attributes: Array[(String, azimuth::AttributeValue)]
  events: Array[(Int, String, Array[(String, azimuth::AttributeValue)])]
  links: Array[(azimuth::SpanContext, Array[(String, azimuth::AttributeValue)])]
  parent_span_id: Option[String]
}

test "跨度创建和基本属性" {
  // 创建跨度上下文
  let span_context = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    sampled: true,
    trace_state: "rojo=00f067aa0ba902b7"
  }
  
  // 创建跨度
  let span = Span {
    name: "http.request",
    context: span_context,
    kind: SpanKind::Server,
    start_time: 1609459200000, // 2021-01-01 00:00:00 UTC
    end_time: None,
    status: StatusCode::Unset,
    attributes: [
      ("http.method", azimuth::AttributeValue::StringValue("GET")),
      ("http.url", azimuth::AttributeValue::StringValue("/api/users")),
      ("http.scheme", azimuth::AttributeValue::StringValue("https"))
    ],
    events: [],
    links: [],
    parent_span_id: Some("parent-span-id-123")
  }
  
  // 验证跨度基本属性
  assert_eq(span.name, "http.request")
  assert_eq(span.context.trace_id, "4bf92f3577b34da6a3ce929d0e0e4736")
  assert_eq(span.context.span_id, "00f067aa0ba902b7")
  assert_true(span.context.sampled)
  
  // 验证跨度类型
  match span.kind {
    SpanKind::Server => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证时间属性
  assert_eq(span.start_time, 1609459200000)
  assert_eq(span.end_time, None)
  
  // 验证状态
  match span.status {
    StatusCode::Unset => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证父跨度ID
  match span.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, "parent-span-id-123")
    None => assert_true(false)
  }
}

test "不同类型的跨度创建" {
  let span_context = azimuth::SpanContext {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331",
    sampled: true,
    trace_state: ""
  }
  
  // 创建服务器跨度
  let server_span = Span {
    name: "server.request",
    context: span_context,
    kind: SpanKind::Server,
    start_time: 1609459200000,
    end_time: None,
    status: StatusCode::Unset,
    attributes: [],
    events: [],
    links: [],
    parent_span_id: None
  }
  
  // 创建客户端跨度
  let client_span = Span {
    name: "client.request",
    context: span_context,
    kind: SpanKind::Client,
    start_time: 1609459200000,
    end_time: None,
    status: StatusCode::Unset,
    attributes: [],
    events: [],
    links: [],
    parent_span_id: None
  }
  
  // 创建生产者跨度
  let producer_span = Span {
    name: "message.publish",
    context: span_context,
    kind: SpanKind::Producer,
    start_time: 1609459200000,
    end_time: None,
    status: StatusCode::Unset,
    attributes: [],
    events: [],
    links: [],
    parent_span_id: None
  }
  
  // 创建消费者跨度
  let consumer_span = Span {
    name: "message.consume",
    context: span_context,
    kind: SpanKind::Consumer,
    start_time: 1609459200000,
    end_time: None,
    status: StatusCode::Unset,
    attributes: [],
    events: [],
    links: [],
    parent_span_id: None
  }
  
  // 创建内部跨度
  let internal_span = Span {
    name: "internal.processing",
    context: span_context,
    kind: SpanKind::Internal,
    start_time: 1609459200000,
    end_time: None,
    status: StatusCode::Unset,
    attributes: [],
    events: [],
    links: [],
    parent_span_id: None
  }
  
  // 验证不同类型的跨度
  match server_span.kind {
    SpanKind::Server => assert_true(true)
    _ => assert_true(false)
  }
  
  match client_span.kind {
    SpanKind::Client => assert_true(true)
    _ => assert_true(false)
  }
  
  match producer_span.kind {
    SpanKind::Producer => assert_true(true)
    _ => assert_true(false)
  }
  
  match consumer_span.kind {
    SpanKind::Consumer => assert_true(true)
    _ => assert_true(false)
  }
  
  match internal_span.kind {
    SpanKind::Internal => assert_true(true)
    _ => assert_true(false)
  }
}

test "跨度属性操作" {
  let span_context = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    sampled: true,
    trace_state: ""
  }
  
  // 创建带有初始属性的跨度
  let mut span = Span {
    name: "database.query",
    context: span_context,
    kind: SpanKind::Client,
    start_time: 1609459200000,
    end_time: None,
    status: StatusCode::Unset,
    attributes: [
      ("db.system", azimuth::AttributeValue::StringValue("postgresql")),
      ("db.name", azimuth::AttributeValue::StringValue("production_db")),
      ("db.statement", azimuth::AttributeValue::StringValue("SELECT * FROM users WHERE id = $1"))
    ],
    events: [],
    links: [],
    parent_span_id: None
  }
  
  // 验证初始属性
  assert_eq(span.attributes.length(), 3)
  
  // 添加新属性
  span.attributes = span.attributes + [
    ("db.user", azimuth::AttributeValue::StringValue("app_user")),
    ("db.port", azimuth::AttributeValue::IntValue(5432))
  ]
  
  // 验证添加后的属性
  assert_eq(span.attributes.length(), 5)
  
  // 验证特定属性
  let mut found_db_system = false
  let mut found_db_user = false
  let mut found_db_port = false
  
  for (key, value) in span.attributes {
    match key {
      "db.system" => {
        found_db_system = true
        match value {
          azimuth::AttributeValue::StringValue(system) => assert_eq(system, "postgresql")
          _ => assert_true(false)
        }
      }
      "db.user" => {
        found_db_user = true
        match value {
          azimuth::AttributeValue::StringValue(user) => assert_eq(user, "app_user")
          _ => assert_true(false)
        }
      }
      "db.port" => {
        found_db_port = true
        match value {
          azimuth::AttributeValue::IntValue(port) => assert_eq(port, 5432)
          _ => assert_true(false)
        }
      }
      _ => () // 其他属性
    }
  }
  
  assert_true(found_db_system)
  assert_true(found_db_user)
  assert_true(found_db_port)
}

test "跨度事件操作" {
  let span_context = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    sampled: true,
    trace_state: ""
  }
  
  // 创建跨度
  let mut span = Span {
    name: "http.request",
    context: span_context,
    kind: SpanKind::Server,
    start_time: 1609459200000,
    end_time: None,
    status: StatusCode::Unset,
    attributes: [],
    events: [],
    links: [],
    parent_span_id: None
  }
  
  // 验证初始事件为空
  assert_eq(span.events.length(), 0)
  
  // 添加事件
  span.events = span.events + [
    (1609459200100, "request.started", [
      ("http.method", azimuth::AttributeValue::StringValue("GET")),
      ("http.url", azimuth::AttributeValue::StringValue("/api/users"))
    ]),
    (1609459200200, "authentication.completed", [
      ("user.id", azimuth::AttributeValue::StringValue("12345")),
      ("auth.method", azimuth::AttributeValue::StringValue("jwt"))
    ]),
    (1609459200300, "database.query.completed", [
      ("db.statement", azimuth::AttributeValue::StringValue("SELECT * FROM users")),
      ("db.rows", azimuth::AttributeValue::IntValue(10))
    ])
  ]
  
  // 验证事件数量
  assert_eq(span.events.length(), 3)
  
  // 验证事件内容
  let (time1, name1, attrs1) = span.events[0]
  assert_eq(time1, 1609459200100)
  assert_eq(name1, "request.started")
  assert_eq(attrs1.length(), 2)
  
  let (time2, name2, attrs2) = span.events[1]
  assert_eq(time2, 1609459200200)
  assert_eq(name2, "authentication.completed")
  assert_eq(attrs2.length(), 2)
  
  let (time3, name3, attrs3) = span.events[2]
  assert_eq(time3, 1609459200300)
  assert_eq(name3, "database.query.completed")
  assert_eq(attrs3.length(), 2)
  
  // 验证事件属性
  match attrs1[0] {
    ("http.method", azimuth::AttributeValue::StringValue(method)) => assert_eq(method, "GET")
    _ => assert_true(false)
  }
  
  match attrs2[0] {
    ("user.id", azimuth::AttributeValue::StringValue(user_id)) => assert_eq(user_id, "12345")
    _ => assert_true(false)
  }
  
  match attrs3[1] {
    ("db.rows", azimuth::AttributeValue::IntValue(rows)) => assert_eq(rows, 10)
    _ => assert_true(false)
  }
}

test "跨度链接操作" {
  let span_context = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    sampled: true,
    trace_state: ""
  }
  
  // 创建相关上下文
  let linked_context1 = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736", // 相同的追踪ID
    span_id: "linked-span-id-1",
    sampled: true,
    trace_state: ""
  }
  
  let linked_context2 = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736", // 相同的追踪ID
    span_id: "linked-span-id-2",
    sampled: true,
    trace_state: ""
  }
  
  // 创建跨度
  let mut span = Span {
    name: "async.processing",
    context: span_context,
    kind: SpanKind::Internal,
    start_time: 1609459200000,
    end_time: None,
    status: StatusCode::Unset,
    attributes: [],
    events: [],
    links: [],
    parent_span_id: None
  }
  
  // 验证初始链接为空
  assert_eq(span.links.length(), 0)
  
  // 添加链接
  span.links = span.links + [
    (linked_context1, [
      ("link.type", azimuth::AttributeValue::StringValue("caused_by")),
      ("operation", azimuth::AttributeValue::StringValue("previous_step"))
    ]),
    (linked_context2, [
      ("link.type", azimuth::AttributeValue::StringValue("related_to")),
      ("component", azimuth::AttributeValue::StringValue("async_queue"))
    ])
  ]
  
  // 验证链接数量
  assert_eq(span.links.length(), 2)
  
  // 验证链接内容
  let (context1, attrs1) = span.links[0]
  assert_eq(context1.span_id, "linked-span-id-1")
  assert_eq(context1.trace_id, "4bf92f3577b34da6a3ce929d0e0e4736")
  assert_eq(attrs1.length(), 2)
  
  let (context2, attrs2) = span.links[1]
  assert_eq(context2.span_id, "linked-span-id-2")
  assert_eq(context2.trace_id, "4bf92f3577b34da6a3ce929d0e0e4736")
  assert_eq(attrs2.length(), 2)
  
  // 验证链接属性
  match attrs1[0] {
    ("link.type", azimuth::AttributeValue::StringValue(link_type)) => assert_eq(link_type, "caused_by")
    _ => assert_true(false)
  }
  
  match attrs2[1] {
    ("component", azimuth::AttributeValue::StringValue(component)) => assert_eq(component, "async_queue")
    _ => assert_true(false)
  }
}

test "跨度状态管理" {
  let span_context = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    sampled: true,
    trace_state: ""
  }
  
  // 创建跨度
  let mut span = Span {
    name: "api.request",
    context: span_context,
    kind: SpanKind::Server,
    start_time: 1609459200000,
    end_time: None,
    status: StatusCode::Unset,
    attributes: [],
    events: [],
    links: [],
    parent_span_id: None
  }
  
  // 验证初始状态
  match span.status {
    StatusCode::Unset => assert_true(true)
    _ => assert_true(false)
  }
  
  // 设置为成功状态
  span.status = StatusCode::Ok
  
  // 验证状态更新
  match span.status {
    StatusCode::Ok => assert_true(true)
    _ => assert_true(false)
  }
  
  // 设置为错误状态
  span.status = StatusCode::Error
  
  // 验证状态更新
  match span.status {
    StatusCode::Error => assert_true(true)
    _ => assert_true(false)
  }
  
  // 重置状态
  span.status = StatusCode::Unset
  
  // 验证状态重置
  match span.status {
    StatusCode::Unset => assert_true(true)
    _ => assert_true(false)
  }
}

test "跨度生命周期" {
  let span_context = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    sampled: true,
    trace_state: ""
  }
  
  // 1. 创建跨度（开始）
  let mut span = Span {
    name: "user.login",
    context: span_context,
    kind: SpanKind::Server,
    start_time: 1609459200000, // 开始时间
    end_time: None, // 尚未结束
    status: StatusCode::Unset,
    attributes: [
      ("user.id", azimuth::AttributeValue::StringValue("12345")),
      ("login.method", azimuth::AttributeValue::StringValue("password"))
    ],
    events: [],
    links: [],
    parent_span_id: None
  }
  
  // 验证跨度已开始
  assert_eq(span.start_time, 1609459200000)
  assert_eq(span.end_time, None)
  
  // 2. 添加事件（进行中）
  span.events = span.events + [
    (1609459200100, "authentication.started", []),
    (1609459200200, "validation.completed", [
      ("validation.result", azimuth::AttributeValue::StringValue("success"))
    ])
  ]
  
  // 验证事件已添加
  assert_eq(span.events.length(), 2)
  
  // 3. 添加更多属性（进行中）
  span.attributes = span.attributes + [
    ("authentication.method", azimuth::AttributeValue::StringValue("bcrypt")),
    ("session.id", azimuth::AttributeValue::StringValue("sess-abc123"))
  ]
  
  // 验证属性已添加
  assert_eq(span.attributes.length(), 4)
  
  // 4. 设置状态（进行中）
  span.status = StatusCode::Ok
  
  // 验证状态已设置
  match span.status {
    StatusCode::Ok => assert_true(true)
    _ => assert_true(false)
  }
  
  // 5. 结束跨度
  span.end_time = Some(1609459200300) // 结束时间
  
  // 验证跨度已结束
  match span.end_time {
    Some(end_time) => assert_eq(end_time, 1609459200300)
    None => assert_true(false)
  }
  
  // 验证跨度持续时间
  let duration = match span.end_time {
    Some(end) => end - span.start_time
    None => 0
  }
  assert_eq(duration, 300) // 300毫秒
}

test "父子跨度关系" {
  // 创建父跨度上下文
  let parent_context = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "parent-span-id",
    sampled: true,
    trace_state: ""
  }
  
  // 创建父跨度
  let parent_span = Span {
    name: "http.request",
    context: parent_context,
    kind: SpanKind::Server,
    start_time: 1609459200000,
    end_time: Some(1609459200500),
    status: StatusCode::Ok,
    attributes: [
      ("http.method", azimuth::AttributeValue::StringValue("POST")),
      ("http.url", azimuth::AttributeValue::StringValue("/api/process"))
    ],
    events: [],
    links: [],
    parent_span_id: None
  }
  
  // 创建子跨度上下文
  let child_context = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736", // 相同的追踪ID
    span_id: "child-span-id",
    sampled: true,
    trace_state: ""
  }
  
  // 创建子跨度
  let child_span = Span {
    name: "database.query",
    context: child_context,
    kind: SpanKind::Client,
    start_time: 1609459200100,
    end_time: Some(1609459200400),
    status: StatusCode::Ok,
    attributes: [
      ("db.system", azimuth::AttributeValue::StringValue("mysql")),
      ("db.statement", azimuth::AttributeValue::StringValue("SELECT * FROM orders"))
    ],
    events: [],
    links: [],
    parent_span_id: Some("parent-span-id") // 指向父跨度
  }
  
  // 验证父子关系
  assert_eq(parent_span.context.trace_id, child_span.context.trace_id) // 相同的追踪ID
  assert_ne(parent_span.context.span_id, child_span.context.span_id) // 不同的跨度ID
  
  // 验证子跨度的父跨度引用
  match child_span.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, "parent-span-id")
    None => assert_true(false)
  }
  
  // 验证父跨度没有父跨度
  assert_eq(parent_span.parent_span_id, None)
  
  // 验证时间关系
  assert_true(parent_span.start_time <= child_span.start_time) // 子跨度在父跨度之后开始
  assert_true(child_span.end_time.unwrap_or(0) <= parent_span.end_time.unwrap_or(0)) // 子跨度在父跨度之前结束
}

test "跨度边界条件测试" {
  // 测试空名称的跨度
  let span_context = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    sampled: true,
    trace_state: ""
  }
  
  let empty_name_span = Span {
    name: "",
    context: span_context,
    kind: SpanKind::Internal,
    start_time: 1609459200000,
    end_time: None,
    status: StatusCode::Unset,
    attributes: [],
    events: [],
    links: [],
    parent_span_id: None
  }
  
  assert_eq(empty_name_span.name, "")
  
  // 测试零时间戳的跨度
  let zero_time_span = Span {
    name: "zero.time.span",
    context: span_context,
    kind: SpanKind::Internal,
    start_time: 0,
    end_time: Some(0),
    status: StatusCode::Unset,
    attributes: [],
    events: [],
    links: [],
    parent_span_id: None
  }
  
  assert_eq(zero_time_span.start_time, 0)
  match zero_time_span.end_time {
    Some(end_time) => assert_eq(end_time, 0)
    None => assert_true(false)
  }
  
  // 测试包含特殊字符的跨度名称
  let special_name_span = Span {
    name: "special.chars.span!@#$%^&*()",
    context: span_context,
    kind: SpanKind::Internal,
    start_time: 1609459200000,
    end_time: None,
    status: StatusCode::Unset,
    attributes: [],
    events: [],
    links: [],
    parent_span_id: None
  }
  
  assert_eq(special_name_span.name, "special.chars.span!@#$%^&*()")
  
  // 测试长跨度名称
  let long_name = "a".repeat(1000)
  let long_name_span = Span {
    name: long_name,
    context: span_context,
    kind: SpanKind::Internal,
    start_time: 1609459200000,
    end_time: None,
    status: StatusCode::Unset,
    attributes: [],
    events: [],
    links: [],
    parent_span_id: None
  }
  
  assert_eq(long_name_span.name.length(), 1000)
  
  // 测试结束时间早于开始时间的跨度（异常情况）
  let invalid_time_span = Span {
    name: "invalid.time.span",
    context: span_context,
    kind: SpanKind::Internal,
    start_time: 1609459200500,
    end_time: Some(1609459200000), // 结束时间早于开始时间
    status: StatusCode::Unset,
    attributes: [],
    events: [],
    links: [],
    parent_span_id: None
  }
  
  assert_true(invalid_time_span.start_time > invalid_time_span.end_time.unwrap_or(0))
}