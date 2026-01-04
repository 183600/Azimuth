// 跨服务追踪测试用例
// 测试Azimuth遥测系统的跨服务追踪功能

test "跨服务上下文传播" {
  // 测试跨服务的上下文传播
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.service.test")
  
  // 在服务A中创建根span
  let service_a_span = Tracer::start_span(tracer, "service.a.operation")
  Span::set_attribute(service_a_span, "service.name", "service-a")
  Span::set_attribute(service_a_span, "service.version", "1.0.0")
  Span::set_attribute(service_a_span, "operation.type", "user.request")
  
  // 获取服务A的span上下文
  let service_a_context = Span::span_context(service_a_span)
  
  // 创建传播器
  let propagator = TextMapPropagator::new()
  let carrier = []
  
  // 将服务A的上下文注入到carrier中
  Propagator::inject(propagator, service_a_context, carrier)
  
  // 验证carrier包含追踪上下文信息
  let traceparent = Carrier::get(carrier, "traceparent")
  let tracestate = Carrier::get(carrier, "tracestate")
  
  assert_true(Option::is_some(traceparent))
  
  // 在服务B中从carrier提取上下文
  let service_b_context = Propagator::extract(propagator, carrier)
  
  // 验证提取的上下文有效
  assert_true(SpanContext::is_valid(service_b_context))
  
  // 验证服务B的上下文与服务A的上下文具有相同的trace ID
  let trace_id_a = SpanContext::trace_id(service_a_context)
  let trace_id_b = SpanContext::trace_id(service_b_context)
  assert_eq(trace_id_a, trace_id_b)
  
  // 在服务B中创建子span
  let service_b_span = Tracer::start_span_with_context(tracer, "service.b.operation", service_b_context)
  Span::set_attribute(service_b_span, "service.name", "service-b")
  Span::set_attribute(service_b_span, "service.version", "2.1.0")
  Span::set_attribute(service_b_span, "operation.type", "data.processing")
  
  // 验证服务B的span是服务A的span的子span
  let service_b_span_context = Span::span_context(service_b_span)
  let parent_span_id = SpanContext::parent_span_id(service_b_span_context)
  let service_a_span_id = SpanContext::span_id(service_a_context)
  
  assert_eq(parent_span_id, service_a_span_id)
  
  Span::end(service_a_span)
  Span::end(service_b_span)
  
  assert_true(true)
}

test "多服务调用链追踪" {
  // 测试多服务调用链的追踪
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "multi.service.chain.test")
  
  // 服务A：API网关
  let gateway_span = Tracer::start_span(tracer, "api.gateway.request")
  Span::set_attribute(gateway_span, "service.name", "api-gateway")
  Span::set_attribute(gateway_span, "http.method", "POST")
  Span::set_attribute(gateway_span, "http.url", "/api/v1/process")
  Span::set_attribute(gateway_span, "user.id", "user-12345")
  
  // 创建传播器并注入上下文
  let propagator = TextMapPropagator::new()
  let carrier_a = []
  let gateway_context = Span::span_context(gateway_span)
  Propagator::inject(propagator, gateway_context, carrier_a)
  
  // 服务B：认证服务
  let auth_context = Propagator::extract(propagator, carrier_a)
  let auth_span = Tracer::start_span_with_context(tracer, "auth.service.validate", auth_context)
  Span::set_attribute(auth_span, "service.name", "auth-service")
  Span::set_attribute(auth_span, "operation.type", "token.validation")
  Span::set_attribute(auth_span, "token.type", "JWT")
  
  // 模拟认证处理
  Span::add_event(auth_span, "token.received", [("token.expiry", "2025-01-02T12:00:00Z")])
  Span::add_event(auth_span, "token.validated", [("validation.result", "success")])
  
  // 传播到服务C
  let carrier_b = []
  let auth_context_final = Span::span_context(auth_span)
  Propagator::inject(propagator, auth_context_final, carrier_b)
  
  // 服务C：业务逻辑服务
  let business_context = Propagator::extract(propagator, carrier_b)
  let business_span = Tracer::start_span_with_context(tracer, "business.service.process", business_context)
  Span::set_attribute(business_span, "service.name", "business-service")
  Span::set_attribute(business_span, "operation.type", "data.processing")
  Span::set_attribute(business_span, "processing.complexity", "high")
  
  // 业务逻辑处理中的子操作
  let db_operation = Tracer::start_span_with_context(tracer, "database.query", Span::span_context(business_span))
  Span::set_attribute(db_operation, "service.name", "business-service")
  Span::set_attribute(db_operation, "operation.type", "database.query")
  Span::set_attribute(db_operation, "db.type", "postgresql")
  Span::set_attribute(db_operation, "db.statement", "SELECT * FROM users WHERE id = ?")
  
  // 模拟数据库查询
  Span::add_event(db_operation, "query.started", [("query.timestamp", "2025-01-02T10:30:15Z")])
  Span::add_event(db_operation, "query.completed", [("query.duration", "45ms"), ("result.count", "1")])
  
  Span::end(db_operation)
  
  // 传播到服务D
  let carrier_c = []
  let business_context_final = Span::span_context(business_span)
  Propagator::inject(propagator, business_context_final, carrier_c)
  
  // 服务D：通知服务
  let notification_context = Propagator::extract(propagator, carrier_c)
  let notification_span = Tracer::start_span_with_context(tracer, "notification.service.send", notification_context)
  Span::set_attribute(notification_span, "service.name", "notification-service")
  Span::set_attribute(notification_span, "operation.type", "email.notification")
  Span::set_attribute(notification_span, "notification.type", "email")
  Span::set_attribute(notification_span, "notification.recipient", "user@example.com")
  
  // 模拟通知发送
  Span::add_event(notification_span, "notification.queued", [("queue.name", "email.outbound")])
  Span::add_event(notification_span, "notification.sent", [("provider", "smtp"), [("message.id", "msg-abc123")])
  
  // 验证整个调用链的追踪ID一致
  let gateway_trace_id = SpanContext::trace_id(gateway_context)
  let auth_trace_id = SpanContext::trace_id(Span::span_context(auth_span))
  let business_trace_id = SpanContext::trace_id(Span::span_context(business_span))
  let notification_trace_id = SpanContext::trace_id(Span::span_context(notification_span))
  
  assert_eq(gateway_trace_id, auth_trace_id)
  assert_eq(auth_trace_id, business_trace_id)
  assert_eq(business_trace_id, notification_trace_id)
  
  Span::end(gateway_span)
  Span::end(auth_span)
  Span::end(business_span)
  Span::end(notification_span)
  
  assert_true(true)
}

test "异步服务调用追踪" {
  // 测试异步服务调用的追踪
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "async.service.test")
  
  // 主服务启动异步操作
  let main_span = Tracer::start_span(tracer, "async.main.operation")
  Span::set_attribute(main_span, "service.name", "async-service")
  Span::set_attribute(main_span, "operation.type", "async.processing")
  
  // 创建传播器
  let propagator = TextMapPropagator::new()
  let main_context = Span::span_context(main_span)
  
  // 启动多个异步子操作
  let async_operations = []
  for i = 0; i < 3; i = i + 1 {
    let carrier = []
    Propagator::inject(propagator, main_context, carrier)
    
    // 模拟异步操作
    let async_context = Propagator::extract(propagator, carrier)
    let async_span = Tracer::start_span_with_context(tracer, "async.operation." + i.to_string(), async_context)
    
    Span::set_attribute(async_span, "service.name", "async-worker-" + i.to_string())
    Span::set_attribute(async_span, "operation.type", "async.task")
    Span::set_attribute(async_span, "task.id", "task-" + i.to_string())
    Span::set_attribute(async_span, "worker.id", "worker-" + i.to_string())
    
    // 模拟异步操作的不同阶段
    Span::add_event(async_span, "task.started", [("start.time", "2025-01-02T10:00:00Z")])
    Span::add_event(async_span, "task.processing", [("progress", "50%")])
    Span::add_event(async_span, "task.completed", [("end.time", "2025-01-02T10:01:30Z")])
    
    async_operations.push(async_span)
  }
  
  // 验证所有异步操作都共享相同的trace ID
  let main_trace_id = SpanContext::trace_id(main_context)
  for async_span in async_operations {
    let async_trace_id = SpanContext::trace_id(Span::span_context(async_span))
    assert_eq(main_trace_id, async_trace_id)
    
    // 验证异步操作是主操作的子操作
    let async_parent_id = SpanContext::parent_span_id(Span::span_context(async_span))
    let main_span_id = SpanContext::span_id(main_context)
    assert_eq(async_parent_id, main_span_id)
  }
  
  // 结束所有异步操作
  for async_span in async_operations {
    Span::end(async_span)
  }
  
  Span::end(main_span)
  
  assert_true(true)
}

test "跨服务错误传播追踪" {
  // 测试跨服务错误传播的追踪
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.service.error.test")
  
  // 服务A：发起请求
  let service_a_span = Tracer::start_span(tracer, "service.a.request")
  Span::set_attribute(service_a_span, "service.name", "service-a")
  Span::set_attribute(service_a_span, "operation.type", "api.request")
  
  // 创建传播器并注入上下文
  let propagator = TextMapPropagator::new()
  let carrier = []
  let service_a_context = Span::span_context(service_a_span)
  Propagator::inject(propagator, service_a_context, carrier)
  
  // 服务B：处理请求并发生错误
  let service_b_context = Propagator::extract(propagator, carrier)
  let service_b_span = Tracer::start_span_with_context(tracer, "service.b.process", service_b_context)
  Span::set_attribute(service_b_span, "service.name", "service-b")
  Span::set_attribute(service_b_span, "operation.type", "data.validation")
  
  // 模拟错误发生
  Span::add_event(service_b_span, "validation.started", [])
  Span::add_event(service_b_span, "validation.error", [
    ("error.type", "ValidationError"),
    ("error.message", "Invalid input parameter"),
    ("error.code", "ERR_001")
  ])
  
  // 设置错误状态
  Span::set_status(service_b_span, Error, Some("Validation failed: Invalid input parameter"))
  
  // 传播错误到服务C
  let carrier_b = []
  let service_b_context_final = Span::span_context(service_b_span)
  Propagator::inject(propagator, service_b_context_final, carrier_b)
  
  // 服务C：处理错误
  let service_c_context = Propagator::extract(propagator, carrier_b)
  let service_c_span = Tracer::start_span_with_context(tracer, "service.c.error.handle", service_c_context)
  Span::set_attribute(service_c_span, "service.name", "service-c")
  Span::set_attribute(service_c_span, "operation.type", "error.handling")
  
  // 记录错误处理过程
  Span::add_event(service_c_span, "error.received", [
    ("source.service", "service-b"),
    ("error.type", "ValidationError")
  ])
  Span::add_event(service_c_span, "error.logged", [])
  Span::add_event(service_c_span, "error.response.generated", [
    ("response.status", "400"),
    ("response.message", "Bad Request")
  ])
  
  // 设置错误处理状态
  Span::set_status(service_c_span, Error, Some("Error handled and response generated"))
  
  // 验证错误追踪链
  let service_a_trace_id = SpanContext::trace_id(service_a_context)
  let service_b_trace_id = SpanContext::trace_id(service_b_context_final)
  let service_c_trace_id = SpanContext::trace_id(Span::span_context(service_c_span))
  
  assert_eq(service_a_trace_id, service_b_trace_id)
  assert_eq(service_b_trace_id, service_c_trace_id)
  
  // 验证错误状态传播
  let service_b_status = Span::status(service_b_span)
  let service_c_status = Span::status(service_c_span)
  
  match service_b_status {
    Status::Error(message) => assert_true(String::contains(message, "Validation failed"))
    _ => assert_true(false)
  }
  
  match service_c_status {
    Status::Error(message) => assert_true(String::contains(message, "Error handled"))
    _ => assert_true(false)
  }
  
  Span::end(service_a_span)
  Span::end(service_b_span)
  Span::end(service_c_span)
  
  assert_true(true)
}

test "跨服务 baggage传播" {
  // 测试跨服务的baggage传播
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.service.baggage.test")
  
  // 服务A：设置baggage
  let service_a_span = Tracer::start_span(tracer, "service.a.operation")
  Span::set_attribute(service_a_span, "service.name", "service-a")
  
  // 创建上下文并添加baggage
  let ctx = Context::root()
  let ctx_with_baggage = Context::with_value(
    ctx,
    BaggageKey::new("user.id"),
    "user-12345"
  )
  let ctx_with_session = Context::with_value(
    ctx_with_baggage,
    BaggageKey::new("session.id"),
    "session-abcdef"
  )
  let ctx_with_tenant = Context::with_value(
    ctx_with_session,
    BaggageKey::new("tenant.id"),
    "tenant-001"
  )
  
  // 创建传播器并注入上下文和baggage
  let propagator = TextMapPropagator::new()
  let carrier = []
  let service_a_context = Span::span_context(service_a_span)
  
  // 注入span上下文
  Propagator::inject(propagator, service_a_context, carrier)
  
  // 注入baggage
  let baggage_propagator = BaggagePropagator::new()
  BaggagePropagator::inject(baggage_propagator, ctx_with_tenant, carrier)
  
  // 验证carrier包含baggage信息
  let baggage_header = Carrier::get(carrier, "baggage")
  assert_true(Option::is_some(baggage_header))
  
  // 服务B：提取上下文和baggage
  let service_b_context = Propagator::extract(propagator, carrier)
  let service_b_baggage = BaggagePropagator::extract(baggage_propagator, carrier)
  
  // 验证提取的baggage
  let user_id = Context::get(service_b_baggage, BaggageKey::new("user.id"))
  let session_id = Context::get(service_b_baggage, BaggageKey::new("session.id"))
  let tenant_id = Context::get(service_b_baggage, BaggageKey::new("tenant.id"))
  
  assert_eq(user_id, Some("user-12345"))
  assert_eq(session_id, Some("session-abcdef"))
  assert_eq(tenant_id, Some("tenant-001"))
  
  // 在服务B中创建span并使用baggage信息
  let service_b_span = Tracer::start_span_with_context(tracer, "service.b.operation", service_b_context)
  Span::set_attribute(service_b_span, "service.name", "service-b")
  
  // 使用baggage信息设置属性
  match user_id {
    Some(id) => Span::set_attribute(service_b_span, "user.id", id)
    None => ()
  }
  match session_id {
    Some(id) => Span::set_attribute(service_b_span, "session.id", id)
    None => ()
  }
  match tenant_id {
    Some(id) => Span::set_attribute(service_b_span, "tenant.id", id)
    None => ()
  }
  
  // 添加新的baggage项
  let service_b_baggage_with_operation = Context::with_value(
    service_b_baggage,
    BaggageKey::new("operation.id"),
    "op-xyz789"
  )
  
  // 传播到服务C
  let carrier_b = []
  let service_b_context_final = Span::span_context(service_b_span)
  
  Propagator::inject(propagator, service_b_context_final, carrier_b)
  BaggagePropagator::inject(baggage_propagator, service_b_baggage_with_operation, carrier_b)
  
  // 服务C：提取并验证所有baggage
  let service_c_context = Propagator::extract(propagator, carrier_b)
  let service_c_baggage = BaggagePropagator::extract(baggage_propagator, carrier_b)
  
  // 验证原始baggage仍然存在
  let c_user_id = Context::get(service_c_baggage, BaggageKey::new("user.id"))
  let c_session_id = Context::get(service_c_baggage, BaggageKey::new("session.id"))
  let c_tenant_id = Context::get(service_c_baggage, BaggageKey::new("tenant.id"))
  
  assert_eq(c_user_id, Some("user-12345"))
  assert_eq(c_session_id, Some("session-abcdef"))
  assert_eq(c_tenant_id, Some("tenant-001"))
  
  // 验证新添加的baggage
  let operation_id = Context::get(service_c_baggage, BaggageKey::new("operation.id"))
  assert_eq(operation_id, Some("op-xyz789"))
  
  Span::end(service_a_span)
  Span::end(service_b_span)
  
  assert_true(true)
}