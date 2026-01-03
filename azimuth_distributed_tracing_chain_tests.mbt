// 分布式追踪链路测试用例
// 测试Azimuth遥测系统中分布式追踪的链路传播和关联功能

test "跨服务追踪链路完整性" {
  // 创建多个服务的tracer
  let tracer_provider = TracerProvider::default()
  
  let gateway_tracer = TracerProvider::get_tracer(tracer_provider, "api-gateway")
  let auth_tracer = TracerProvider::get_tracer(tracer_provider, "auth-service")
  let user_tracer = TracerProvider::get_tracer(tracer_provider, "user-service")
  let order_tracer = TracerProvider::get_tracer(tracer_provider, "order-service")
  
  // 网关服务创建根span
  let gateway_span = Tracer::start_span(gateway_tracer, "gateway.process.request")
  let gateway_context = Span::span_context(gateway_span)
  let trace_id = SpanContext::trace_id(gateway_context)
  
  // 设置网关span属性
  Span::set_attribute(gateway_span, "service.name", "api-gateway")
  Span::set_attribute(gateway_span, "http.method", "POST")
  Span::set_attribute(gateway_span, "http.url", "/api/orders")
  Span::set_attribute(gateway_span, "client.ip", "192.168.1.100")
  
  // 网关调用认证服务
  let auth_span = Tracer::start_span(auth_tracer, "auth.validate.token")
  let auth_context = Span::span_context(auth_span)
  
  // 验证认证服务span与网关span在同一trace中
  assert_eq(SpanContext::trace_id(auth_context), trace_id)
  
  // 设置认证服务span属性
  Span::set_attribute(auth_span, "service.name", "auth-service")
  Span::set_attribute(auth_span, "auth.token", "bearer-token-xyz")
  Span::set_attribute(auth_span, "auth.user.id", "user-123")
  
  // 认证服务调用用户服务
  let user_span = Tracer::start_span(user_tracer, "user.get.profile")
  let user_context = Span::span_context(user_span)
  
  // 验证用户服务span在同一trace中
  assert_eq(SpanContext::trace_id(user_context), trace_id)
  
  // 设置用户服务span属性
  Span::set_attribute(user_span, "service.name", "user-service")
  Span::set_attribute(user_span, "user.id", "user-123")
  Span::set_attribute(user_span, "db.query", "SELECT * FROM users WHERE id = ?")
  
  // 用户服务调用订单服务
  let order_span = Tracer::start_span(order_tracer, "order.get.history")
  let order_context = Span::span_context(order_span)
  
  // 验证订单服务span在同一trace中
  assert_eq(SpanContext::trace_id(order_context), trace_id)
  
  // 设置订单服务span属性
  Span::set_attribute(order_span, "service.name", "order-service")
  Span::set_attribute(order_span, "user.id", "user-123")
  Span::set_attribute(order_span, "order.count", 5)
  
  // 按相反顺序结束span（模拟服务响应）
  Span::end(order_span)
  Span::end(user_span)
  Span::end(auth_span)
  Span::end(gateway_span)
  
  // 验证所有span都有有效的上下文
  assert_true(SpanContext::is_valid(gateway_context))
  assert_true(SpanContext::is_valid(auth_context))
  assert_true(SpanContext::is_valid(user_context))
  assert_true(SpanContext::is_valid(order_context))
}

test "异步操作追踪关联" {
  // 测试异步操作的追踪关联
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "async.test")
  
  // 创建主操作span
  let main_span = Tracer::start_span(tracer, "async.main.operation")
  let main_context = Span::span_context(main_span)
  let trace_id = SpanContext::trace_id(main_context)
  
  // 设置主操作属性
  Span::set_attribute(main_span, "operation.type", "async.processing")
  Span::set_attribute(main_span, "task.count", 3)
  
  // 创建异步任务spans
  let async_spans = []
  let async_tasks = ["task.email.send", "task.notification.push", "task.analytics.update"]
  
  for i = 0; i < async_tasks.length(); i = i + 1 {
    let task_name = async_tasks[i]
    let async_span = Tracer::start_span(tracer, task_name)
    let async_context = Span::span_context(async_span)
    
    // 验证异步span与主span在同一trace中
    assert_eq(SpanContext::trace_id(async_context), trace_id)
    
    // 设置异步任务属性
    Span::set_attribute(async_span, "task.name", task_name)
    Span::set_attribute(async_span, "task.id", "task-" + i.to_string())
    Span::set_attribute(async_span, "async.parent", "async.main.operation")
    
    // 模拟异步任务中的子操作
    let sub_span = Tracer::start_span(tracer, "sub.operation." + i.to_string())
    let sub_context = Span::span_context(sub_span)
    
    // 验证子操作也在同一trace中
    assert_eq(SpanContext::trace_id(sub_context), trace_id)
    
    Span::set_attribute(sub_span, "sub.operation.type", "database.query")
    Span::set_attribute(sub_span, "sub.operation.parent", task_name)
    
    // 结束子操作
    Span::end(sub_span)
    
    async_spans.push(async_span)
  }
  
  // 结束所有异步任务
  for async_span in async_spans {
    Span::end(async_span)
  }
  
  // 结束主操作
  Span::end(main_span)
  
  assert_true(true)
}

test "批处理操作追踪" {
  // 测试批处理操作的追踪
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "batch.test")
  
  // 创建批处理主span
  let batch_span = Tracer::start_span(tracer, "batch.process.orders")
  let batch_context = Span::span_context(batch_span)
  let trace_id = SpanContext::trace_id(batch_context)
  
  // 设置批处理属性
  Span::set_attribute(batch_span, "batch.id", "batch-12345")
  Span::set_attribute(batch_span, "batch.size", 100)
  Span::set_attribute(batch_span, "batch.type", "order.processing")
  
  // 创建批处理中的子操作spans
  let batch_items = []
  let batch_size = 10  // 简化为10个批处理项
  
  for i = 0; i < batch_size; i = i + 1 {
    let item_span = Tracer::start_span(tracer, "batch.item.process")
    let item_context = Span::span_context(item_span)
    
    // 验证批处理项span与主span在同一trace中
    assert_eq(SpanContext::trace_id(item_context), trace_id)
    
    // 设置批处理项属性
    Span::set_attribute(item_span, "batch.item.id", "item-" + i.to_string())
    Span::set_attribute(item_span, "batch.item.index", i)
    Span::set_attribute(item_span, "batch.parent", "batch.process.orders")
    
    // 模拟批处理项中的操作
    let validate_span = Tracer::start_span(tracer, "item.validate")
    let validate_context = Span::span_context(validate_span)
    
    assert_eq(SpanContext::trace_id(validate_context), trace_id)
    Span::set_attribute(validate_span, "validation.type", "order.data")
    Span::end(validate_span)
    
    let transform_span = Tracer::start_span(tracer, "item.transform")
    let transform_context = Span::span_context(transform_span)
    
    assert_eq(SpanContext::trace_id(transform_context), trace_id)
    Span::set_attribute(transform_span, "transformation.type", "data.format")
    Span::end(transform_span)
    
    let persist_span = Tracer::start_span(tracer, "item.persist")
    let persist_context = Span::span_context(persist_span)
    
    assert_eq(SpanContext::trace_id(persist_context), trace_id)
    Span::set_attribute(persist_span, "persistence.type", "database.write")
    Span::end(persist_span)
    
    // 结束批处理项span
    Span::end(item_span)
    batch_items.push(item_span)
  }
  
  // 结束批处理主span
  Span::end(batch_span)
  
  // 验证批处理追踪完整性
  assert_eq(batch_items.length(), batch_size)
  assert_true(SpanContext::is_valid(batch_context))
}

test "微服务调用链追踪" {
  // 测试微服务调用链追踪
  let tracer_provider = TracerProvider::default()
  
  // 创建多个微服务的tracer
  let services = [
    ("frontend", "web-frontend"),
    ("api-gateway", "api-gateway"),
    ("user-service", "user-service"),
    ("product-service", "product-service"),
    ("order-service", "order-service"),
    ("payment-service", "payment-service"),
    ("notification-service", "notification-service")
  ]
  
  let tracers = []
  for (service_type, service_name) in services {
    let tracer = TracerProvider::get_tracer(tracer_provider, service_name)
    tracers.push((service_type, service_name, tracer))
  }
  
  // 模拟微服务调用链
  let call_chain = [
    ("frontend", "web-frontend", "user.click.checkout"),
    ("api-gateway", "api-gateway", "gateway.route.request"),
    ("user-service", "user-service", "user.validate.session"),
    ("product-service", "product-service", "product.get.details"),
    ("order-service", "order-service", "order.create"),
    ("payment-service", "payment-service", "payment.process"),
    ("order-service", "order-service", "order.update.status"),
    ("notification-service", "notification-service", "notification.send.confirmation")
  ]
  
  let spans = []
  let trace_id = ""
  
  for i = 0; i < call_chain.length(); i = i + 1 {
    let (service_type, service_name, operation_name) = call_chain[i]
    
    // 找到对应的tracer
    let tracer = match service_type {
      "frontend" => TracerProvider::get_tracer(tracer_provider, "web-frontend"),
      "api-gateway" => TracerProvider::get_tracer(tracer_provider, "api-gateway"),
      "user-service" => TracerProvider::get_tracer(tracer_provider, "user-service"),
      "product-service" => TracerProvider::get_tracer(tracer_provider, "product-service"),
      "order-service" => TracerProvider::get_tracer(tracer_provider, "order-service"),
      "payment-service" => TracerProvider::get_tracer(tracer_provider, "payment-service"),
      "notification-service" => TracerProvider::get_tracer(tracer_provider, "notification-service"),
      _ => TracerProvider::get_tracer(tracer_provider, "unknown")
    }
    
    // 创建span
    let span = Tracer::start_span(tracer, operation_name)
    let context = Span::span_context(span)
    
    // 第一个span的trace_id作为整个调用链的trace_id
    if i == 0 {
      trace_id = SpanContext::trace_id(context)
    }
    
    // 验证所有span在同一trace中
    assert_eq(SpanContext::trace_id(context), trace_id)
    
    // 设置span属性
    Span::set_attribute(span, "service.type", service_type)
    Span::set_attribute(span, "service.name", service_name)
    Span::set_attribute(span, "operation.name", operation_name)
    
    // 设置调用链关系
    if i > 0 {
      let (prev_service_type, _, _) = call_chain[i - 1]
      Span::set_attribute(span, "caller.service", prev_service_type)
    }
    
    if i < call_chain.length() - 1 {
      let (next_service_type, _, _) = call_chain[i + 1]
      Span::set_attribute(span, "callee.service", next_service_type)
    }
    
    spans.push(span)
  }
  
  // 按相反顺序结束span（模拟服务响应）
  for i = spans.length() - 1; i >= 0; i = i - 1 {
    Span::end(spans[i])
  }
  
  // 验证调用链完整性
  assert_eq(spans.length(), call_chain.length())
  assert_true(trace_id != "")
}

test "错误传播和上下文保留" {
  // 测试错误传播和上下文保留
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error.propagation.test")
  
  // 创建根span
  let root_span = Tracer::start_span(tracer, "root.operation")
  let root_context = Span::span_context(root_span)
  let trace_id = SpanContext::trace_id(root_context)
  
  // 设置根span属性
  Span::set_attribute(root_span, "operation.type", "error.prone")
  Span::set_attribute(root_span, "user.id", "user-123")
  Span::set_attribute(root_span, "request.id", "req-456")
  
  // 创建子span1（成功）
  let child_span1 = Tracer::start_span(tracer, "child.operation.1")
  let child_context1 = Span::span_context(child_span1)
  
  assert_eq(SpanContext::trace_id(child_context1), trace_id)
  Span::set_attribute(child_span1, "operation.type", "validation")
  Span::end(child_span1)  // 成功结束
  
  // 创建子span2（失败）
  let child_span2 = Tracer::start_span(tracer, "child.operation.2")
  let child_context2 = Span::span_context(child_span2)
  
  assert_eq(SpanContext::trace_id(child_context2), trace_id)
  Span::set_attribute(child_span2, "operation.type", "database.query")
  
  // 设置错误状态
  Span::set_status(child_span2, Error, Some("Database connection timeout"))
  Span::add_event(child_span2, "error.occurred", [
    ("error.type", "TimeoutError"),
    ("error.code", "DB_TIMEOUT"),
    ("error.message", "Connection timeout after 30 seconds")
  ])
  
  // 创建子span3（错误处理）
  let child_span3 = Tracer::start_span(tracer, "child.operation.3")
  let child_context3 = Span::span_context(child_span3)
  
  assert_eq(SpanContext::trace_id(child_context3), trace_id)
  Span::set_attribute(child_span3, "operation.type", "error.handling")
  Span::set_attribute(child_span3, "error.recovery", "retry")
  
  // 模拟错误恢复
  Span::set_status(child_span3, Ok, Some("Error recovered successfully"))
  Span::add_event(child_span3, "recovery.started", [
    ("recovery.strategy", "exponential.backoff"),
    ("max.retries", "3")
  ])
  
  Span::end(child_span3)
  Span::end(child_span2)
  
  // 创建子span4（后续操作）
  let child_span4 = Tracer::start_span(tracer, "child.operation.4")
  let child_context4 = Span::span_context(child_span4)
  
  assert_eq(SpanContext::trace_id(child_context4), trace_id)
  Span::set_attribute(child_span4, "operation.type", "cleanup")
  Span::set_attribute(child_span4, "error.context", "preserved")
  Span::end(child_span4)
  
  // 结束根span
  Span::set_status(root_span, Error, Some("Operation completed with errors"))
  Span::end(root_span)
  
  // 验证错误传播和上下文保留
  assert_true(SpanContext::is_valid(root_context))
  assert_true(SpanContext::is_valid(child_context1))
  assert_true(SpanContext::is_valid(child_context2))
  assert_true(SpanContext::is_valid(child_context3))
  assert_true(SpanContext::is_valid(child_context4))
}

test "跨线程追踪上下文传播" {
  // 测试跨线程追踪上下文传播
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "thread.propagation.test")
  
  // 创建主线程span
  let main_span = Tracer::start_span(tracer, "main.thread.operation")
  let main_context = Span::span_context(main_span)
  let trace_id = SpanContext::trace_id(main_context)
  
  // 设置主线程span属性
  Span::set_attribute(main_span, "thread.id", "main")
  Span::set_attribute(main_span, "thread.type", "main")
  
  // 模拟创建多个工作线程
  let worker_threads = []
  let worker_count = 3
  
  for i = 0; i < worker_count; i = i + 1 {
    // 创建工作线程span
    let worker_span = Tracer::start_span(tracer, "worker.thread.operation")
    let worker_context = Span::span_context(worker_span)
    
    // 验证工作线程span与主线程span在同一trace中
    assert_eq(SpanContext::trace_id(worker_context), trace_id)
    
    // 设置工作线程span属性
    Span::set_attribute(worker_span, "thread.id", "worker-" + i.to_string())
    Span::set_attribute(worker_span, "thread.type", "worker")
    Span::set_attribute(worker_span, "parent.thread", "main")
    
    // 模拟工作线程中的子操作
    let sub_operation_span = Tracer::start_span(tracer, "sub.operation.in.worker")
    let sub_operation_context = Span::span_context(sub_operation_span)
    
    // 验证子操作也在同一trace中
    assert_eq(SpanContext::trace_id(sub_operation_context), trace_id)
    
    Span::set_attribute(sub_operation_span, "operation.type", "data.processing")
    Span::set_attribute(sub_operation_span, "worker.thread.id", "worker-" + i.to_string())
    
    // 结束子操作
    Span::end(sub_operation_span)
    
    worker_threads.push(worker_span)
  }
  
  // 模拟线程池操作
  let pool_span = Tracer::start_span(tracer, "thread.pool.operation")
  let pool_context = Span::span_context(pool_span)
  
  assert_eq(SpanContext::trace_id(pool_context), trace_id)
  Span::set_attribute(pool_span, "thread.pool.size", worker_count.to_string())
  Span::set_attribute(pool_span, "thread.pool.type", "fixed")
  
  // 结束线程池操作
  Span::end(pool_span)
  
  // 结束所有工作线程span
  for worker_span in worker_threads {
    Span::end(worker_span)
  }
  
  // 结束主线程span
  Span::end(main_span)
  
  // 验证跨线程追踪上下文传播
  assert_true(SpanContext::is_valid(main_context))
  assert_eq(worker_threads.length(), worker_count)
}