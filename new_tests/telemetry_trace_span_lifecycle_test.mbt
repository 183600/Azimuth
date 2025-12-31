// 追踪Span生命周期测试
// 测试Span从创建到结束的完整生命周期

test "span_creation_with_all_parameters" {
  // 测试使用所有参数创建Span
  let ctx = context::Context::empty()
  let tracer = trace::NoopTracer::{}
  
  let (new_ctx, span) = tracer.start_span(
    ctx,
    "http_request",
    Some(trace::Server),
    Some([
      ("http.method", common::AttributeValue::string("GET")),
      ("http.url", common::AttributeValue::string("/api/users")),
      ("http.scheme", common::AttributeValue::string("https"))
    ]),
    Some(1640995200000000000L)  // 2022-01-01 00:00:00 UTC
  )
  
  // 验证Span基本属性
  assert_eq(span.name, "http_request")
  assert match span.kind {
    trace::Server => true
    _ => false
  }
  assert_eq(span.start_time_unix_nanos, 1640995200000000000L)
  assert_eq(span.end_time_unix_nanos, None)
  assert match span.status {
    trace::Unset => true
    _ => false
  }
  
  // 验证属性
  assert_eq(span.attributes.length(), 3)
  assert_eq(span.attributes[0].0, "http.method")
  assert_eq(span.attributes[1].0, "http.url")
  assert_eq(span.attributes[2].0, "http.scheme")
  
  // 验证SpanContext
  assert_eq(span.context.trace_id.length(), 16)
  assert_eq(span.context.span_id.length(), 8)
  assert_eq(span.context.trace_flags, 0_byte)
  assert_eq(span.context.trace_state, "")
}

test "span_with_different_kinds" {
  // 测试不同类型的Span
  let ctx = context::Context::empty()
  let tracer = trace::NoopTracer::{}
  
  // 测试所有SpanKind
  let kinds = [
    trace::Internal,
    trace::Server,
    trace::Client,
    trace::Producer,
    trace::Consumer
  ]
  
  let kind_names = ["Internal", "Server", "Client", "Producer", "Consumer"]
  let mut i = 0
  
  while i < kinds.length() {
    let (_, span) = tracer.start_span(
      ctx,
      "test_span_" + i.to_string(),
      Some(kinds[i]),
      None,
      None
    )
    
    assert match span.kind {
      kinds[i] => true
      _ => false
    }
    
    i = i + 1
  }
}

test "span_context_properties" {
  // 测试SpanContext的详细属性
  let ctx = context::Context::empty()
  let tracer = trace::NoopTracer::{}
  
  let (_, span) = tracer.start_span(ctx, "test_span", None, None, None)
  
  let span_ctx = span.context
  
  // 验证trace_id和span_id的长度
  assert_eq(span_ctx.trace_id.length(), 16)
  assert_eq(span_ctx.span_id.length(), 8)
  
  // 验证所有字节都是0（NoopTracer的实现）
  let mut j = 0
  while j < span_ctx.trace_id.length() {
    assert_eq(span_ctx.trace_id[j], 0_byte)
    j = j + 1
  }
  
  j = 0
  while j < span_ctx.span_id.length() {
    assert_eq(span_ctx.span_id[j], 0_byte)
    j = j + 1
  }
  
  // 验证其他属性
  assert_eq(span_ctx.trace_flags, 0_byte)
  assert_eq(span_ctx.trace_state, "")
}

test "span_with_events_and_links" {
  // 测试包含事件和链接的Span
  let ctx = context::Context::empty()
  let tracer = trace::NoopTracer::{}
  
  // 创建一个父SpanContext用于链接
  let parent_span_context = trace::SpanContext::{
    trace_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte],
    span_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte],
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  // 创建Span链接
  let span_link = trace::SpanLink::{
    context: parent_span_context,
    attributes: [
      ("link.type", common::AttributeValue::string("parent")),
      ("link.reason", common::AttributeValue::string("causality"))
    ]
  }
  
  // 创建带链接的Span
  let (_, span) = tracer.start_span(
    ctx,
    "child_span",
    Some(trace::Client),
    Some([
      ("operation.type", common::AttributeValue::string("database_query"))
    ]),
    None
  )
  
  // 模拟添加事件和链接（在实际实现中这些会是可变操作）
  let span_with_links = trace::Span::{
    name: span.name,
    context: span.context,
    kind: span.kind,
    parent_span_id: Some(parent_span_context.span_id),
    start_time_unix_nanos: span.start_time_unix_nanos,
    end_time_unix_nanos: span.end_time_unix_nanos,
    status: span.status,
    status_description: span.status_description,
    attributes: span.attributes,
    events: [
      trace::SpanEvent::{
        name: "database.query.started",
        timestamp_unix_nanos: 1640995200000000000L,
        attributes: [
          ("db.statement", common::AttributeValue::string("SELECT * FROM users")),
          ("db.type", common::AttributeValue::string("postgresql"))
        ]
      },
      trace::SpanEvent::{
        name: "database.query.completed",
        timestamp_unix_nanos: 1640995200000000500L,
        attributes: [
          ("db.rows_affected", common::AttributeValue::int(10L)),
          ("db.duration_ms", common::AttributeValue::float(0.5))
        ]
      }
    ],
    links: [span_link]
  }
  
  // 验证事件
  assert_eq(span_with_links.events.length(), 2)
  assert_eq(span_with_links.events[0].name, "database.query.started")
  assert_eq(span_with_links.events[1].name, "database.query.completed")
  assert_eq(span_with_links.events[0].attributes.length(), 2)
  assert_eq(span_with_links.events[1].attributes.length(), 2)
  
  // 验证链接
  assert_eq(span_with_links.links.length(), 1)
  assert_eq(span_with_links.links[0].context.trace_id, parent_span_context.trace_id)
  assert_eq(span_with_links.links[0].context.span_id, parent_span_context.span_id)
  assert_eq(span_with_links.links[0].attributes.length(), 2)
}

test "span_status_transitions" {
  // 测试Span状态转换
  let ctx = context::Context::empty()
  let tracer = trace::NoopTracer::{}
  
  let (_, span) = tracer.start_span(ctx, "status_test_span", None, None, None)
  
  // 初始状态应该是Unset
  assert match span.status {
    trace::Unset => true
    _ => false
  }
  assert_eq(span.status_description, None)
  
  // 模拟状态转换到Ok
  let span_ok = trace::Span::{
    name: span.name,
    context: span.context,
    kind: span.kind,
    parent_span_id: span.parent_span_id,
    start_time_unix_nanos: span.start_time_unix_nanos,
    end_time_unix_nanos: Some(1640995200000001000L),
    status: trace::Ok,
    status_description: Some("Operation completed successfully"),
    attributes: span.attributes,
    events: span.events,
    links: span.links
  }
  
  assert match span_ok.status {
    trace::Ok => true
    _ => false
  }
  assert match span_ok.status_description {
    Some(desc) => desc == "Operation completed successfully"
    None => false
  }
  assert_eq(span_ok.end_time_unix_nanos, Some(1640995200000001000L))
  
  // 模拟状态转换到Error
  let span_error = trace::Span::{
    name: span.name,
    context: span.context,
    kind: span.kind,
    parent_span_id: span.parent_span_id,
    start_time_unix_nanos: span.start_time_unix_nanos,
    end_time_unix_nanos: Some(1640995200000002000L),
    status: trace::Error,
    status_description: Some("Database connection failed"),
    attributes: span.attributes,
    events: span.events,
    links: span.links
  }
  
  assert match span_error.status {
    trace::Error => true
    _ => false
  }
  assert match span_error.status_description {
    Some(desc) => desc == "Database connection failed"
    None => false
  }
  assert_eq(span_error.end_time_unix_nanos, Some(1640995200000002000L))
}

test "span_with_complex_attributes" {
  // 测试包含复杂属性的Span
  let ctx = context::Context::empty()
  let tracer = trace::NoopTracer::{}
  
  let complex_attributes = [
    ("service.name", common::AttributeValue::string("payment-service")),
    ("service.version", common::AttributeValue::string("2.1.0")),
    ("service.namespace", common::AttributeValue::string("ecommerce")),
    ("deployment.environment", common::AttributeValue::string("production")),
    ("host.name", common::AttributeValue::string("payment-server-01")),
    ("host.ip", common::AttributeValue::string("10.0.1.100")),
    ("process.pid", common::AttributeValue::int(12345L)),
    ("process.executable.name", common::AttributeValue::string("payment-service")),
    ("process.command_args", common::AttributeValue::array_string(["./payment-service", "--config", "/etc/payment/config.yaml"])),
    ("process.cpu.percent", common::AttributeValue::float(75.5)),
    ("process.memory.rss", common::AttributeValue::float(512.0)),
    ("process.healthy", common::AttributeValue::bool(true)),
    ("http.method", common::AttributeValue::string("POST")),
    ("http.scheme", common::AttributeValue::string("https")),
    ("http.target", common::AttributeValue::string("/api/v2/payments")),
    ("http.status_code", common::AttributeValue::int(200L)),
    ("http.response.size", common::AttributeValue::int(1024L)),
    ("user.id", common::AttributeValue::string("user-12345")),
    ("user.tier", common::AttributeValue::string("premium")),
    ("payment.amount", common::AttributeValue::float(99.99)),
    ("payment.currency", common::AttributeValue::string("USD")),
    ("payment.status", common::AttributeValue::string("completed")),
    ("payment.method", common::AttributeValue::string("credit_card")),
    ("tags", common::AttributeValue::array_string(["api", "payment", "critical", "revenue"])),
    ("status_codes", common::AttributeValue::array_int([200L, 201L, 400L, 401L, 500L])),
    ("latency.thresholds", common::AttributeValue::array_float([0.1, 0.5, 1.0, 2.5])),
    ("feature.flags", common::AttributeValue::array_bool([true, false, true, true, false]))
  ]
  
  let (_, span) = tracer.start_span(
    ctx,
    "complex_payment_span",
    Some(trace::Server),
    Some(complex_attributes),
    Some(1640995200000000000L)
  )
  
  // 验证属性数量
  assert_eq(span.attributes.length(), 30)
  
  // 验证特定属性
  assert_eq(span.attributes[0].0, "service.name")
  match span.attributes[0].1 {
    common::StringValue(name) => assert_eq(name, "payment-service")
    _ => @test.fail("Test failed")
  }
  
  assert_eq(span.attributes[7].0, "process.pid")
  match span.attributes[7].1 {
    common::IntValue(pid) => assert_eq(pid, 12345L)
    _ => @test.fail("Test failed")
  }
  
  assert_eq(span.attributes[10].0, "process.cpu.percent")
  match span.attributes[10].1 {
    common::FloatValue(cpu) => assert_eq(cpu, 75.5)
    _ => @test.fail("Test failed")
  }
  
  assert_eq(span.attributes[11].0, "process.healthy")
  match span.attributes[11].1 {
    common::BoolValue(healthy) => assert_eq(healthy, true)
    _ => @test.fail("Test failed")
  }
  
  // 验证数组属性
  assert_eq(span.attributes[8].0, "process.command_args")
  match span.attributes[8].1 {
    common::ArrayStringValue(args) => {
      assert_eq(args.length(), 3)
      assert_eq(args[0], "./payment-service")
      assert_eq(args[1], "--config")
      assert_eq(args[2], "/etc/payment/config.yaml")
    }
    _ => @test.fail("Test failed")
  }
}

test "tracer_provider_lifecycle" {
  // 测试TracerProvider的生命周期
  let provider = trace::NoopTracerProvider::{}
  
  // 创建多个Tracer
  let payment_tracer = provider.get_tracer("payment-service", Some("2.1.0"))
  let auth_tracer = provider.get_tracer("auth-service", Some("1.5.2"))
  let notification_tracer = provider.get_tracer("notification-service", None)
  
  // 使用不同的Tracer创建Span
  let ctx = context::Context::empty()
  
  let (_, payment_span) = payment_tracer.start_span(
    ctx,
    "payment_processing",
    Some(trace::Server),
    Some([
      ("service.name", common::AttributeValue::string("payment-service")),
      ("service.version", common::AttributeValue::string("2.1.0"))
    ]),
    None
  )
  
  let (_, auth_span) = auth_tracer.start_span(
    ctx,
    "user_authentication",
    Some(trace::Server),
    Some([
      ("service.name", common::AttributeValue::string("auth-service")),
      ("service.version", common::AttributeValue::string("1.5.2"))
    ]),
    None
  )
  
  let (_, notification_span) = notification_tracer.start_span(
    ctx,
    "email_notification",
    Some(trace::Producer),
    Some([
      ("service.name", common::AttributeValue::string("notification-service")),
      ("notification.type", common::AttributeValue::string("email"))
    ]),
    None
  )
  
  // 验证不同Tracer创建的Span
  assert_eq(payment_span.name, "payment_processing")
  assert_eq(auth_span.name, "user_authentication")
  assert_eq(notification_span.name, "email_notification")
  
  assert match payment_span.kind {
    trace::Server => true
    _ => false
  }
  
  assert match auth_span.kind {
    trace::Server => true
    _ => false
  }
  
  assert match notification_span.kind {
    trace::Producer => true
    _ => false
  }
  
  // 验证服务属性
  assert_eq(payment_span.attributes.length(), 2)
  assert_eq(auth_span.attributes.length(), 2)
  assert_eq(notification_span.attributes.length(), 2)
}