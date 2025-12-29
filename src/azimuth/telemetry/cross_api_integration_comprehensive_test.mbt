// 跨API集成测试 - 测试Trace、Metrics、Logs、Context之间的深度集成

test "trace_metrics_logs_integration" {
  // 创建完整的遥测场景，包含trace、metrics、logs和context
  
  // 1. 设置Context和Baggage
  let ctx = Context::empty()
  let trace_key = create_key("trace.id")
  let user_key = create_key("user.id")
  let request_key = create_key("request.id")
  
  let ctx_with_values = ctx
    .with_value(trace_key, "trace-12345")
    .with_value(user_key, "user-67890")
    .with_value(request_key, "req-abcdef")
  
  let baggage = Baggage::empty()
    .with_entry("service.name", "payment-service")
    .with_entry("service.version", "2.1.0")
    .with_entry("deployment.env", "production")
    .with_entry("request.source", "api-gateway")
  
  // 2. 创建Trace Span
  let (_, payment_span) = NoopTracer::start_span(
    ctx_with_values, 
    "payment.process",
    Server,
    Some([
      ("service.operation", AttributeValue::string("process_payment")),
      ("payment.method", AttributeValue::string("credit_card")),
      ("payment.amount", AttributeValue::float(99.99)),
      ("user.tier", AttributeValue::string("premium"))
    ])
  )
  
  // 3. 创建Metrics仪器
  let meter = NoopMeter::{}
  let payment_counter = meter.create_counter(
    "payments_total",
    Some("payments"),
    Some("Total number of payments processed")
  )
  let payment_duration = meter.create_histogram(
    "payment_duration_seconds",
    Some("seconds"),
    Some("Payment processing duration")
  )
  let active_payments = meter.create_up_down_counter(
    "active_payments",
    Some("payments"),
    Some("Currently active payments")
  )
  let payment_queue_size = meter.create_gauge(
    "payment_queue_size",
    Some("payments"),
    Some("Current payment queue size")
  )
  
  // 4. 记录Metrics
  let payment_attributes = [
    ("service.name", AttributeValue::string("payment-service")),
    ("payment.method", AttributeValue::string("credit_card")),
    ("payment.currency", AttributeValue::string("USD")),
    ("user.tier", AttributeValue::string("premium")),
    ("region", AttributeValue::string("us-east-1")),
    ("tags", AttributeValue::array_string(["api", "payment", "critical"]))
  ]
  
  active_payments.add(1L, Some(payment_attributes))
  payment_queue_size.record(5.0, Some(payment_attributes))
  payment_duration.record(2.5, Some(payment_attributes))
  
  // 5. 创建Logger并记录日志
  let logger = NoopLogger::{}
  
  // 处理过程中的日志
  logger.info("Payment processing started", Some([
    ("payment.id", AttributeValue::string("pay-12345")),
    ("user.id", AttributeValue::string("user-67890")),
    ("amount", AttributeValue::float(99.99)),
    ("currency", AttributeValue::string("USD")),
    ("trace.id", AttributeValue::string("trace-12345"))
  ]))
  
  logger.debug("Validating payment method", Some([
    ("payment.method", AttributeValue::string("credit_card")),
    ("validation.status", AttributeValue::string("in_progress"))
  ]))
  
  // 6. 模拟支付处理完成
  payment_counter.add(1L, Some(payment_attributes))
  active_payments.add(-1L, Some(payment_attributes))
  payment_queue_size.record(4.0, Some(payment_attributes))
  
  logger.info("Payment processed successfully", Some([
    ("payment.id", AttributeValue::string("pay-12345")),
    ("status", AttributeValue::string("completed")),
    ("processing.time.ms", AttributeValue::int(2500L)),
    ("trace.id", AttributeValue::string("trace-12345"))
  ]))
  
  // 7. 验证所有组件都能正确工作
  assert_eq(payment_span.name, "payment.process")
  assert_eq(payment_span.kind, Server)
  assert_eq(payment_span.attributes.length(), 4)
  
  // 验证Context中的值
  match ctx_with_values.get(trace_key) {
    Some(value) => assert_eq(value, "trace-12345")
    None => @test.fail("Test failed")
  }
  
  match ctx_with_values.get(user_key) {
    Some(value) => assert_eq(value, "user-67890")
    None => @test.fail("Test failed")
  }
  
  // 验证Baggage中的值
  match baggage.get("service.name") {
    Some(value) => assert_eq(value, "payment-service")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("deployment.env") {
    Some(value) => assert_eq(value, "production")
    None => @test.fail("Test failed")
  }
}

test "context_propagation_across_apis" {
  // 测试Context在Trace、Metrics、Logs之间的传播
  
  // 1. 创建初始Context
  let ctx = Context::empty()
  let correlation_key = create_key("correlation.id")
  let user_key = create_key("user.id")
  let session_key = create_key("session.id")
  
  let initial_ctx = ctx
    .with_value(correlation_key, "corr-12345")
    .with_value(user_key, "user-67890")
    .with_value(session_key, "sess-abcdef")
  
  // 2. 在Trace中使用Context
  let (_, auth_span) = NoopTracer::start_span(
    initial_ctx,
    "user.authenticate",
    Server,
    Some([
      ("service.name", AttributeValue::string("auth-service")),
      ("operation.type", AttributeValue::string("authenticate")),
      ("correlation.id", AttributeValue::string("corr-12345"))
    ])
  )
  
  // 3. 创建带有Context信息的Metrics
  let meter = NoopMeter::{}
  let auth_counter = meter.create_counter("auth_attempts_total", Some("attempts"), Some("Total authentication attempts"))
  
  let auth_attributes = [
    ("service.name", AttributeValue::string("auth-service")),
    ("correlation.id", AttributeValue::string("corr-12345")),
    ("user.id", AttributeValue::string("user-67890")),
    ("auth.method", AttributeValue::string("password"))
  ]
  
  auth_counter.add(1L, Some(auth_attributes))
  
  // 4. 使用Context信息记录日志
  let logger = NoopLogger::{}
  
  logger.info("Authentication attempt", Some([
    ("correlation.id", AttributeValue::string("corr-12345")),
    ("user.id", AttributeValue::string("user-67890")),
    ("session.id", AttributeValue::string("sess-abcdef")),
    ("auth.method", AttributeValue::string("password")),
    ("service.name", AttributeValue::string("auth-service"))
  ]))
  
  // 5. 模拟认证成功，创建新的Context
  let success_ctx = initial_ctx.with_value(create_key("auth.status"), "success")
  
  let (_, token_span) = NoopTracer::start_span(
    success_ctx,
    "token.generate",
    Server,
    Some([
      ("service.name", AttributeValue::string("auth-service")),
      ("operation.type", AttributeValue::string("generate_token")),
      ("correlation.id", AttributeValue::string("corr-12345"))
    ])
  )
  
  // 6. 记录成功的Metrics和日志
  let success_attributes = [
    ("service.name", AttributeValue::string("auth-service")),
    ("correlation.id", AttributeValue::string("corr-12345")),
    ("user.id", AttributeValue::string("user-67890")),
    ("auth.status", AttributeValue::string("success"))
  ]
  
  let token_counter = meter.create_counter("tokens_generated_total", Some("tokens"), Some("Total tokens generated"))
  token_counter.add(1L, Some(success_attributes))
  
  logger.info("Token generated successfully", Some([
    ("correlation.id", AttributeValue::string("corr-12345")),
    ("user.id", AttributeValue::string("user-67890")),
    ("token.type", AttributeValue::string("bearer")),
    ("token.expiry", AttributeValue::int(3600L)),
    ("auth.status", AttributeValue::string("success"))
  ]))
  
  // 7. 验证Context传播的一致性
  assert_eq(auth_span.name, "user.authenticate")
  assert_eq(token_span.name, "token.generate")
  
  // 验证correlation ID在整个流程中保持一致
  match initial_ctx.get(correlation_key) {
    Some(value) => assert_eq(value, "corr-12345")
    None => @test.fail("Test failed")
  }
  
  match success_ctx.get(correlation_key) {
    Some(value) => assert_eq(value, "corr-12345")
    None => @test.fail("Test failed")
  }
  
  // 验证新的Context值
  match success_ctx.get(create_key("auth.status")) {
    Some(value) => assert_eq(value, "success")
    None => @test.fail("Test failed")
  }
}

test "baggage_cross_api_integration" {
  // 测试Baggage在不同API之间的集成使用
  
  // 1. 创建复杂的Baggage
  let baggage = Baggage::empty()
    .with_entry("user.id", "user-12345")
    .with_entry("user.tier", "premium")
    .with_entry("session.id", "sess-abcdef")
    .with_entry("request.id", "req-12345")
    .with_entry("service.name", "order-service")
    .with_entry("service.version", "1.5.2")
    .with_entry("deployment.env", "production")
    .with_entry("region", "us-west-2")
    .with_entry("trace.sampled", "true")
    .with_entry("debug.enabled", "false")
  
  // 2. 创建包含Baggage信息的Context
  let ctx = Context::empty()
  let baggage_key = create_key("baggage")
  let ctx_with_baggage = ctx.with_value(baggage_key, "user.id=user-12345,user.tier=premium")
  
  // 3. 在Trace中使用Baggage信息
  let trace_attributes = [
    ("service.name", AttributeValue::string("order-service")),
    ("user.id", AttributeValue::string("user-12345")),
    ("user.tier", AttributeValue::string("premium")),
    ("session.id", AttributeValue::string("sess-abcdef")),
    ("region", AttributeValue::string("us-west-2"))
  ]
  
  let (_, order_span) = NoopTracer::start_span(
    ctx_with_baggage,
    "order.create",
    Server,
    Some(trace_attributes)
  )
  
  // 4. 在Metrics中使用Baggage信息
  let meter = NoopMeter::{}
  let order_counter = meter.create_counter("orders_total", Some("orders"), Some("Total orders created"))
  
  let order_attributes = [
    ("service.name", AttributeValue::string("order-service")),
    ("user.id", AttributeValue::string("user-12345")),
    ("user.tier", AttributeValue::string("premium")),
    ("region", AttributeValue::string("us-west-2")),
    ("deployment.env", AttributeValue::string("production"))
  ]
  
  order_counter.add(1L, Some(order_attributes))
  
  let order_amount_histogram = meter.create_histogram("order_amount", Some("dollars"), Some("Order amount in dollars"))
  order_amount_histogram.record(149.99, Some(order_attributes))
  
  // 5. 在Logs中使用Baggage信息
  let logger = NoopLogger::{}
  
  logger.info("Order created", Some([
    ("service.name", AttributeValue::string("order-service")),
    ("order.id", AttributeValue::string("order-67890")),
    ("user.id", AttributeValue::string("user-12345")),
    ("user.tier", AttributeValue::string("premium")),
    ("session.id", AttributeValue::string("sess-abcdef")),
    ("request.id", AttributeValue::string("req-12345")),
    ("order.amount", AttributeValue::float(149.99)),
    ("currency", AttributeValue::string("USD")),
    ("region", AttributeValue::string("us-west-2"))
  ]))
  
  // 6. 验证Baggage信息在整个流程中的一致性
  assert_eq(order_span.name, "order.create")
  assert_eq(order_span.attributes.length(), 5)
  
  // 验证Baggage中的关键信息
  match baggage.get("user.id") {
    Some(value) => assert_eq(value, "user-12345")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("user.tier") {
    Some(value) => assert_eq(value, "premium")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("region") {
    Some(value) => assert_eq(value, "us-west-2")
    None => @test.fail("Test failed")
  }
  
  // 7. 测试Baggage的传播和更新
  let updated_baggage = baggage
    .with_entry("order.id", "order-67890")
    .with_entry("order.status", "created")
    .with_entry("payment.status", "pending")
  
  match updated_baggage.get("order.id") {
    Some(value) => assert_eq(value, "order-67890")
    None => @test.fail("Test failed")
  }
  
  match updated_baggage.get("user.id") {
    Some(value) => assert_eq(value, "user-12345")  // 原有值应该保持
    None => @test.fail("Test failed")
  }
}

test "comprehensive_telemetry_workflow" {
  // 综合遥测工作流测试，模拟完整的微服务调用链
  
  // 1. API Gateway - 接收请求
  let gateway_ctx = Context::empty()
    .with_value(create_key("request.id"), "req-gateway-001")
    .with_value(create_key("client.ip"), "192.168.1.100")
    .with_value(create_key("user.agent"), "mobile-app/1.0")
  
  let gateway_baggage = Baggage::empty()
    .with_entry("service.name", "api-gateway")
    .with_entry("service.version", "2.0.0")
    .with_entry("request.source", "mobile")
    .with_entry("user.id", "user-mobile-123")
  
  let (_, gateway_span) = NoopTracer::start_span(
    gateway_ctx,
    "api.gateway.request",
    Server,
    Some([
      ("service.name", AttributeValue::string("api-gateway")),
      ("http.method", AttributeValue::string("POST")),
      ("http.path", AttributeValue::string("/api/v1/orders")),
      ("client.ip", AttributeValue::string("192.168.1.100")),
      ("user.id", AttributeValue::string("user-mobile-123"))
    ])
  )
  
  let gateway_logger = NoopLogger::{}
  gateway_logger.info("Request received at API Gateway", Some([
    ("request.id", AttributeValue::string("req-gateway-001")),
    ("http.method", AttributeValue::string("POST")),
    ("http.path", AttributeValue::string("/api/v1/orders")),
    ("client.ip", AttributeValue::string("192.168.1.100"))
  ]))
  
  // 2. Auth Service - 身份验证
  let auth_ctx = gateway_ctx
    .with_value(create_key("auth.required"), "true")
    .with_value(create_key("auth.token"), "bearer-token-xyz")
  
  let (_, auth_span) = NoopTracer::start_span(
    auth_ctx,
    "auth.validate",
    Client,
    Some([
      ("service.name", AttributeValue::string("auth-service")),
      ("auth.method", AttributeValue::string("bearer")),
      ("user.id", AttributeValue::string("user-mobile-123"))
    ])
  )
  
  let auth_meter = NoopMeter::{}
  let auth_counter = auth_meter.create_counter("auth_validations_total", Some("validations"), Some("Total auth validations"))
  auth_counter.add(1L, Some([
    ("service.name", AttributeValue::string("auth-service")),
    ("auth.result", AttributeValue::string("success")),
    ("user.id", AttributeValue::string("user-mobile-123"))
  ]))
  
  // 3. Order Service - 创建订单
  let order_ctx = auth_ctx
    .with_value(create_key("order.payload"), "json-payload")
    .with_value(create_key("order.id"), "order-xyz-789")
  
  let (_, order_span) = NoopTracer::start_span(
    order_ctx,
    "order.create",
    Client,
    Some([
      ("service.name", AttributeValue::string("order-service")),
      ("operation.type", AttributeValue::string("create")),
      ("order.id", AttributeValue::string("order-xyz-789")),
      ("user.id", AttributeValue::string("user-mobile-123"))
    ])
  )
  
  let order_meter = NoopMeter::{}
  let order_counter = order_meter.create_counter("orders_created_total", Some("orders"), Some("Total orders created"))
  order_counter.add(1L, Some([
    ("service.name", AttributeValue::string("order-service")),
    ("user.id", AttributeValue::string("user-mobile-123")),
    ("order.type", AttributeValue::string("standard"))
  ]))
  
  let order_amount_histogram = order_meter.create_histogram("order_amount", Some("dollars"), Some("Order amount"))
  order_amount_histogram.record(89.99, Some([
    ("service.name", AttributeValue::string("order-service")),
    ("user.tier", AttributeValue::string("standard")),
    ("currency", AttributeValue::string("USD"))
  ]))
  
  // 4. Payment Service - 处理支付
  let payment_ctx = order_ctx
    .with_value(create_key("payment.method"), "credit_card")
    .with_value(create_key("payment.amount"), "89.99")
  
  let (_, payment_span) = NoopTracer::start_span(
    payment_ctx,
    "payment.process",
    Client,
    Some([
      ("service.name", AttributeValue::string("payment-service")),
      ("payment.method", AttributeValue::string("credit_card")),
      ("payment.amount", AttributeValue::float(89.99)),
      ("currency", AttributeValue::string("USD"))
    ])
  )
  
  let payment_meter = NoopMeter::{}
  let payment_counter = payment_meter.create_counter("payments_processed_total", Some("payments"), Some("Total payments processed"))
  payment_counter.add(1L, Some([
    ("service.name", AttributeValue::string("payment-service")),
    ("payment.method", AttributeValue::string("credit_card")),
    ("payment.result", AttributeValue::string("success"))
  ]))
  
  let payment_duration_histogram = payment_meter.create_histogram("payment_duration_ms", Some("ms"), Some("Payment processing duration"))
  payment_duration_histogram.record(1250.0, Some([
    ("service.name", AttributeValue::string("payment-service")),
    ("payment.method", AttributeValue::string("credit_card"))
  ]))
  
  // 5. Inventory Service - 库存检查
  let inventory_ctx = payment_ctx
    .with_value(create_key("inventory.checked"), "true")
    .with_value(create_key("inventory.available"), "true")
  
  let (_, inventory_span) = NoopTracer::start_span(
    inventory_ctx,
    "inventory.check",
    Client,
    Some([
      ("service.name", AttributeValue::string("inventory-service")),
      ("operation.type", AttributeValue::string("check_availability")),
      ("product.id", AttributeValue::string("prod-123")),
      ("quantity", AttributeValue::int(1L))
    ])
  )
  
  // 6. Notification Service - 发送通知
  let notification_ctx = inventory_ctx
    .with_value(create_key("notification.type"), "email")
    .with_value(create_key("notification.recipient"), "user@example.com")
  
  let (_, notification_span) = NoopTracer::start_span(
    notification_ctx,
    "notification.send",
    Client,
    Some([
      ("service.name", AttributeValue::string("notification-service")),
      ("notification.type", AttributeValue::string("email")),
      ("notification.status", AttributeValue::string("sent"))
    ])
  )
  
  // 7. 最终日志记录
  let final_logger = NoopLogger::{}
  final_logger.info("Order processing completed successfully", Some([
    ("request.id", AttributeValue::string("req-gateway-001")),
    ("order.id", AttributeValue::string("order-xyz-789")),
    ("user.id", AttributeValue::string("user-mobile-123")),
    ("total.amount", AttributeValue::float(89.99)),
    ("payment.method", AttributeValue::string("credit_card")),
    ("services.involved", AttributeValue::array_string(["api-gateway", "auth-service", "order-service", "payment-service", "inventory-service", "notification-service"])),
    ("total.duration.ms", AttributeValue::int(3200L))
  ]))
  
  // 8. 验证整个工作流
  assert_eq(gateway_span.name, "api.gateway.request")
  assert_eq(auth_span.name, "auth.validate")
  assert_eq(order_span.name, "order.create")
  assert_eq(payment_span.name, "payment.process")
  assert_eq(inventory_span.name, "inventory.check")
  assert_eq(notification_span.name, "notification.send")
  
  // 验证Context传播的一致性
  match gateway_ctx.get(create_key("request.id")) {
    Some(value) => assert_eq(value, "req-gateway-001")
    None => @test.fail("Test failed")
  }
  
  match order_ctx.get(create_key("order.id")) {
    Some(value) => assert_eq(value, "order-xyz-789")
    None => @test.fail("Test failed")
  }
  
  match payment_ctx.get(create_key("payment.amount")) {
    Some(value) => assert_eq(value, "89.99")
    None => @test.fail("Test failed")
  }
}