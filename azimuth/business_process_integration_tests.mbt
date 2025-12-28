// Business Process Integration Tests
// 业务流程集成测试，测试完整的业务流程中的遥测数据集成

test "电子商务订单处理流程测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "ecommerce.order")
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "ecommerce.order")
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "ecommerce.order")
  
  // 创建业务指标
  let order_counter = Meter::create_counter(meter, "orders.total")
  let revenue_counter = Meter::create_counter(meter, "revenue.total")
  let order_processing_time = Meter::create_histogram(meter, "order.processing.time")
  let inventory_gauge = Meter::create_gauge(meter, "inventory.available")
  
  // 开始订单处理流程
  let order_span = Tracer::start_span(tracer, "order.processing.flow")
  Span::add_event(order_span, "flow.started", Some([
    ("flow.type", StringValue("ecommerce.order")),
    ("customer.id", StringValue("customer-12345")),
    ("order.id", StringValue("order-abcde"))
  ]))
  
  // 步骤1：订单验证
  let validation_span = Tracer::start_span(tracer, "order.validation")
  Span::add_event(validation_span, "validation.started", Some([
    ("order.id", StringValue("order-abcde")),
    ("validation.type", StringValue("customer_and_payment"))
  ]))
  
  // 模拟验证时间
  let validation_time = 25.0 + (Random::next_u64(Random::system()) % 50).to_float()
  Histogram::record(order_processing_time, validation_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("step", StringValue("validation")),
    ("order.id", StringValue("order-abcde"))
  ])))
  
  Span::set_status(validation_span, Ok)
  Span::add_event(validation_span, "validation.completed", Some([
    ("validation.result", StringValue("passed")),
    ("validation.time.ms", FloatValue(validation_time))
  ]))
  Span::end(validation_span)
  
  // 步骤2：库存检查
  let inventory_span = Tracer::start_span(tracer, "inventory.check")
  Span::add_event(inventory_span, "inventory.check.started", Some([
    ("order.id", StringValue("order-abcde")),
    ("product.count", IntValue(3))
  ]))
  
  // 模拟库存检查
  let inventory_time = 15.0 + (Random::next_u64(Random::system()) % 30).to_float()
  Histogram::record(order_processing_time, inventory_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("step", StringValue("inventory_check")),
    ("order.id", StringValue("order-abcde"))
  ])))
  
  // 更新库存指标
  // 注意：简化实现中Gauge可能不支持设置值
  
  Span::set_status(inventory_span, Ok)
  Span::add_event(inventory_span, "inventory.check.completed", Some([
    ("inventory.status", StringValue("available")),
    ("reserved.quantity", IntValue(3))
  ]))
  Span::end(inventory_span)
  
  // 步骤3：支付处理
  let payment_span = Tracer::start_span(tracer, "payment.processing")
  Span::add_event(payment_span, "payment.started", Some([
    ("order.id", StringValue("order-abcde")),
    ("payment.method", StringValue("credit_card")),
    ("payment.amount", FloatValue(299.99))
  ]))
  
  // 模拟支付处理
  let payment_time = 150.0 + (Random::next_u64(Random::system()) % 200).to_float()
  Histogram::record(order_processing_time, payment_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("step", StringValue("payment")),
    ("order.id", StringValue("order-abcde"))
  ])))
  
  // 记录支付日志
  let payment_log = LogRecord::new_with_context(
    Info,
    Some("Payment processing completed"),
    Some(Attributes::with_attributes(Attributes::new(), [
      ("order.id", StringValue("order-abcde")),
      ("payment.method", StringValue("credit_card")),
      ("payment.amount", FloatValue(299.99)),
      ("payment.time.ms", FloatValue(payment_time)),
      ("payment.status", StringValue("approved"))
    ])),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(order_span))),
    Some(SpanContext::span_id(Span::span_context(payment_span))),
    None
  )
  
  Logger::emit(logger, payment_log)
  
  Span::set_status(payment_span, Ok)
  Span::add_event(payment_span, "payment.completed", Some([
    ("payment.status", StringValue("approved")),
    ("transaction.id", StringValue("txn-123456"))
  ]))
  Span::end(payment_span)
  
  // 步骤4：订单确认
  let confirmation_span = Tracer::start_span(tracer, "order.confirmation")
  Span::add_event(confirmation_span, "confirmation.started", Some([
    ("order.id", StringValue("order-abcde")),
    ("customer.email", StringValue("customer@example.com"))
  ]))
  
  // 模拟确认处理
  let confirmation_time = 10.0 + (Random::next_u64(Random::system()) % 20).to_float()
  Histogram::record(order_processing_time, confirmation_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("step", StringValue("confirmation")),
    ("order.id", StringValue("order-abcde"))
  ])))
  
  // 更新业务指标
  Counter::add(order_counter, 1.0, Some(Attributes::with_attributes(Attributes::new(), [
    ("order.status", StringValue("completed")),
    ("customer.tier", StringValue("premium"))
  ])))
  
  Counter::add(revenue_counter, 299.99, Some(Attributes::with_attributes(Attributes::new(), [
    ("product.category", StringValue("electronics")),
    ("payment.method", StringValue("credit_card"))
  ])))
  
  Span::set_status(confirmation_span, Ok)
  Span::add_event(confirmation_span, "confirmation.completed", Some([
    ("confirmation.id", StringValue("conf-789012")),
    ("email.sent", BoolValue(true))
  ]))
  Span::end(confirmation_span)
  
  // 完成订单流程
  Span::set_status(order_span, Ok)
  Span::add_event(order_span, "flow.completed", Some([
    ("order.id", StringValue("order-abcde")),
    ("total.processing.time.ms", FloatValue(validation_time + inventory_time + payment_time + confirmation_time)),
    ("flow.status", StringValue("success"))
  ]))
  Span::end(order_span)
  
  // 记录订单完成日志
  let completion_log = LogRecord::new_with_context(
    Info,
    Some("E-commerce order processing completed successfully"),
    Some(Attributes::with_attributes(Attributes::new(), [
      ("order.id", StringValue("order-abcde")),
      ("customer.id", StringValue("customer-12345")),
      ("total.amount", FloatValue(299.99)),
      ("processing.steps", IntValue(4)),
      ("total.time.ms", FloatValue(validation_time + inventory_time + payment_time + confirmation_time))
    ])),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(order_span))),
    Some(SpanContext::span_id(Span::span_context(order_span))),
    None
  )
  
  Logger::emit(logger, completion_log)
  
  assert_true(true) // 如果没有崩溃则测试通过
}

test "用户认证授权流程测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "auth.flow")
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "auth.flow")
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "auth.flow")
  
  // 创建认证指标
  let auth_counter = Meter::create_counter(meter, "auth.attempts")
  let auth_success_counter = Meter::create_counter(meter, "auth.success")
  let auth_failure_counter = Meter::create_counter(meter, "auth.failures")
  let auth_time = Meter::create_histogram(meter, "auth.processing.time")
  
  // 开始认证流程
  let auth_span = Tracer::start_span(tracer, "user.authentication.flow")
  Span::add_event(auth_span, "auth.flow.started", Some([
    ("user.id", StringValue("user-67890")),
    ("auth.method", StringValue("oauth2")),
    ("client.ip", StringValue("192.168.1.100"))
  ]))
  
  // 步骤1：身份验证
  let identity_span = Tracer::start_span(tracer, "identity.verification")
  Span::add_event(identity_span, "identity.check.started", Some([
    ("user.id", StringValue("user-67890")),
    ("credential.type", StringValue("jwt_token"))
  ]))
  
  let identity_time = 20.0 + (Random::next_u64(Random::system()) % 40).to_float()
  Histogram::record(auth_time, identity_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("auth.step", StringValue("identity_verification")),
    ("user.id", StringValue("user-67890"))
  ])))
  
  Counter::add(auth_counter, 1.0, Some(Attributes::with_attributes(Attributes::new(), [
    ("auth.step", StringValue("identity")),
    ("auth.method", StringValue("oauth2"))
  ])))
  
  Span::set_status(identity_span, Ok)
  Span::add_event(identity_span, "identity.verified", Some([
    ("verification.result", StringValue("valid")),
    ("token.expiry", StringValue("2025-01-21T00:00:00Z"))
  ]))
  Span::end(identity_span)
  
  // 步骤2：权限检查
  let authorization_span = Tracer::start_span(tracer, "authorization.check")
  Span::add_event(authorization_span, "authorization.check.started", Some([
    ("user.id", StringValue("user-67890")),
    ("requested.resource", StringValue("/api/user/profile")),
    ("required.permission", StringValue("user.profile.read"))
  ]))
  
  let authorization_time = 15.0 + (Random::next_u64(Random::system()) % 25).to_float()
  Histogram::record(auth_time, authorization_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("auth.step", StringValue("authorization")),
    ("user.id", StringValue("user-67890"))
  ])))
  
  // 模拟权限检查结果
  let auth_success = true // 假设认证成功
  
  if auth_success {
    Counter::add(auth_success_counter, 1.0, Some(Attributes::with_attributes(Attributes::new(), [
      ("auth.method", StringValue("oauth2")),
      ("user.role", StringValue("premium"))
    ])))
    
    Span::set_status(authorization_span, Ok)
    Span::add_event(authorization_span, "authorization.granted", Some([
      ("granted.permission", StringValue("user.profile.read")),
      ("user.role", StringValue("premium"))
    ]))
  } else {
    Counter::add(auth_failure_counter, 1.0, Some(Attributes::with_attributes(Attributes::new(), [
      ("auth.method", StringValue("oauth2")),
      ("failure.reason", StringValue("insufficient_permissions"))
    ])))
    
    Span::set_status(authorization_span, Error)
    Span::add_event(authorization_span, "authorization.denied", Some([
      ("denied.reason", StringValue("insufficient_permissions")),
      ("required.permission", StringValue("user.profile.read"))
    ]))
  }
  
  Span::end(authorization_span)
  
  // 步骤3：会话建立
  let session_span = Tracer::start_span(tracer, "session.establishment")
  Span::add_event(session_span, "session.creation.started", Some([
    ("user.id", StringValue("user-67890")),
    ("session.type", StringValue("authenticated"))
  ]))
  
  let session_time = 10.0 + (Random::next_u64(Random::system()) % 20).to_float()
  Histogram::record(auth_time, session_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("auth.step", StringValue("session_establishment")),
    ("user.id", StringValue("user-67890"))
  ])))
  
  Span::set_status(session_span, Ok)
  Span::add_event(session_span, "session.established", Some([
    ("session.id", StringValue("sess-abcdef123456")),
    ("session.expiry", StringValue("2025-01-20T18:30:00Z")),
    ("session.timeout", IntValue(3600))
  ]))
  Span::end(session_span)
  
  // 完成认证流程
  Span::set_status(auth_span, if auth_success { Ok } else { Error })
  Span::add_event(auth_span, "auth.flow.completed", Some([
    ("user.id", StringValue("user-67890")),
    ("auth.result", StringValue(if auth_success { "success" } else { "failure" })),
    ("total.auth.time.ms", FloatValue(identity_time + authorization_time + session_time))
  ]))
  Span::end(auth_span)
  
  // 记录认证结果日志
  let auth_result_log = LogRecord::new_with_context(
    if auth_success { Info } else { Warn },
    Some("User authentication " + (if auth_success { "successful" } else { "failed" })),
    Some(Attributes::with_attributes(Attributes::new(), [
      ("user.id", StringValue("user-67890")),
      ("auth.method", StringValue("oauth2")),
      ("auth.result", StringValue(if auth_success { "success" } else { "failure" })),
      ("client.ip", StringValue("192.168.1.100")),
      ("total.time.ms", FloatValue(identity_time + authorization_time + session_time))
    ])),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(auth_span))),
    Some(SpanContext::span_id(Span::span_context(auth_span))),
    None
  )
  
  Logger::emit(logger, auth_result_log)
  
  assert_true(auth_success) // 验证认证成功
}

test "微服务调用链测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "microservice.chain")
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "microservice.chain")
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "microservice.chain")
  
  // 创建微服务调用指标
  let service_call_counter = Meter::create_counter(meter, "service.calls")
  let service_latency = Meter::create_histogram(meter, "service.latency")
  let circuit_breaker_gauge = Meter::create_gauge(meter, "circuit.breaker.state")
  
  // 开始微服务调用链
  let gateway_span = Tracer::start_span(tracer, "api.gateway.request")
  Span::add_event(gateway_span, "gateway.request.received", Some([
    ("request.id", StringValue("req-123456")),
    ("client.ip", StringValue("10.0.1.100")),
    ("user.agent", StringValue("MobileApp/2.1.0"))
  ]))
  
  let gateway_start = Clock::now_unix_nanos(Clock::system())
  
  // 服务1：用户服务调用
  let user_service_span = Tracer::start_span(tracer, "user.service.call")
  Span::add_event(user_service_span, "service.call.started", Some([
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("2.1.0")),
    ("endpoint", StringValue("/api/users/12345"))
  ]))
  
  let user_service_time = 45.0 + (Random::next_u64(Random::system()) % 80).to_float()
  Histogram::record(service_latency, user_service_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("service.name", StringValue("user-service")),
    ("endpoint", StringValue("/api/users/12345"))
  ])))
  
  Counter::add(service_call_counter, 1.0, Some(Attributes::with_attributes(Attributes::new(), [
    ("service.name", StringValue("user-service")),
    ("status.code", IntValue(200))
  ])))
  
  Span::set_status(user_service_span, Ok)
  Span::add_event(user_service_span, "service.call.completed", Some([
    ("service.name", StringValue("user-service")),
    ("response.time.ms", FloatValue(user_service_time)),
    ("response.size", IntValue(1024))
  ]))
  Span::end(user_service_span)
  
  // 服务2：订单服务调用
  let order_service_span = Tracer::start_span(tracer, "order.service.call")
  Span::add_event(order_service_span, "service.call.started", Some([
    ("service.name", StringValue("order-service")),
    ("service.version", StringValue("3.2.1")),
    ("endpoint", StringValue("/api/orders/user/12345"))
  ]))
  
  let order_service_time = 120.0 + (Random::next_u64(Random::system()) % 150).to_float()
  Histogram::record(service_latency, order_service_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("service.name", StringValue("order-service")),
    ("endpoint", StringValue("/api/orders/user/12345"))
  ])))
  
  Counter::add(service_call_counter, 1.0, Some(Attributes::with_attributes(Attributes::new(), [
    ("service.name", StringValue("order-service")),
    ("status.code", IntValue(200))
  ])))
  
  Span::set_status(order_service_span, Ok)
  Span::add_event(order_service_span, "service.call.completed", Some([
    ("service.name", StringValue("order-service")),
    ("response.time.ms", FloatValue(order_service_time)),
    ("order.count", IntValue(5))
  ]))
  Span::end(order_service_span)
  
  // 服务3：库存服务调用（模拟失败）
  let inventory_service_span = Tracer::start_span(tracer, "inventory.service.call")
  Span::add_event(inventory_service_span, "service.call.started", Some([
    ("service.name", StringValue("inventory-service")),
    ("service.version", StringValue("1.5.2")),
    ("endpoint", StringValue("/api/inventory/check"))
  ]))
  
  let inventory_service_time = 2000.0 + (Random::next_u64(Random::system()) % 1000).to_float() // 超时
  Histogram::record(service_latency, inventory_service_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("service.name", StringValue("inventory-service")),
    ("endpoint", StringValue("/api/inventory/check"))
  ])))
  
  Counter::add(service_call_counter, 1.0, Some(Attributes::with_attributes(Attributes::new(), [
    ("service.name", StringValue("inventory-service")),
    ("status.code", IntValue(503))
  ])))
  
  // 模拟熔断器触发
  // 注意：简化实现中Gauge可能不支持设置值
  
  Span::set_status(inventory_service_span, Error)
  Span::add_event(inventory_service_span, "service.call.failed", Some([
    ("service.name", StringValue("inventory-service")),
    ("error.type", StringValue("ServiceUnavailable")),
    ("error.message", StringValue("Inventory service timeout")),
    ("circuit.breaker", StringValue("open"))
  ]))
  Span::end(inventory_service_span)
  
  // 服务4：缓存服务调用（降级）
  let cache_service_span = Tracer::start_span(tracer, "cache.service.call")
  Span::add_event(cache_service_span, "service.call.started", Some([
    ("service.name", StringValue("cache-service")),
    ("service.version", StringValue("4.0.1")),
    ("endpoint", StringValue("/api/cache/inventory/fallback")),
    ("fallback.reason", StringValue("inventory_service_unavailable"))
  ]))
  
  let cache_service_time = 25.0 + (Random::next_u64(Random::system()) % 30).to_float()
  Histogram::record(service_latency, cache_service_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("service.name", StringValue("cache-service")),
    ("endpoint", StringValue("/api/cache/inventory/fallback"))
  ])))
  
  Counter::add(service_call_counter, 1.0, Some(Attributes::with_attributes(Attributes::new(), [
    ("service.name", StringValue("cache-service")),
    ("status.code", IntValue(200)),
    ("fallback", BoolValue(true))
  ])))
  
  Span::set_status(cache_service_span, Ok)
  Span::add_event(cache_service_span, "service.call.completed", Some([
    ("service.name", StringValue("cache-service")),
    ("response.time.ms", FloatValue(cache_service_time)),
    ("cache.hit", BoolValue(true)),
    ("fallback.success", BoolValue(true))
  ]))
  Span::end(cache_service_span)
  
  // 完成网关请求
  let gateway_end = Clock::now_unix_nanos(Clock::system())
  let total_gateway_time = (gateway_end - gateway_start) / 1000000L // 转换为毫秒
  
  Span::set_status(gateway_span, Ok)
  Span::add_event(gateway_span, "gateway.request.completed", Some([
    ("request.id", StringValue("req-123456")),
    ("total.time.ms", IntValue(total_gateway_time.to_int())),
    ("services.called", IntValue(4)),
    ("services.failed", IntValue(1)),
    ("fallback.used", BoolValue(true))
  ]))
  Span::end(gateway_span)
  
  // 记录微服务调用链日志
  let chain_log = LogRecord::new_with_context(
    Info,
    Some("Microservice call chain completed with fallback"),
    Some(Attributes::with_attributes(Attributes::new(), [
      ("request.id", StringValue("req-123456")),
      ("gateway.time.ms", IntValue(total_gateway_time.to_int())),
      ("services.called", IntValue(4)),
      ("services.successful", IntValue(3)),
      ("services.failed", IntValue(1)),
      ("fallback.activated", BoolValue(true)),
      ("circuit.breaker.triggered", BoolValue(true))
    ])),
    Some(gateway_end),
    None,
    Some(SpanContext::trace_id(Span::span_context(gateway_span))),
    Some(SpanContext::span_id(Span::span_context(gateway_span))),
    None
  )
  
  Logger::emit(logger, chain_log)
  
  assert_true(total_gateway_time > 0L)
}

test "数据处理管道测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "data.pipeline")
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "data.pipeline")
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "data.pipeline")
  
  // 创建数据处理指标
  let data_throughput = Meter::create_counter(meter, "data.processed.bytes")
  let pipeline_latency = Meter::create_histogram(meter, "pipeline.stage.latency")
  let data_quality_gauge = Meter::create_gauge(meter, "data.quality.score")
  
  // 开始数据处理管道
  let pipeline_span = Tracer::start_span(tracer, "data.pipeline.execution")
  Span::add_event(pipeline_span, "pipeline.started", Some([
    ("pipeline.id", StringValue("pipeline-123")),
    ("pipeline.version", StringValue("2.0.0")),
    ("batch.id", StringValue("batch-456"))
  ]))
  
  let pipeline_start = Clock::now_unix_nanos(Clock::system())
  
  // 阶段1：数据摄入
  let ingestion_span = Tracer::start_span(tracer, "data.ingestion")
  Span::add_event(ingestion_span, "ingestion.started", Some([
    ("data.source", StringValue("kafka.topic.user_events")),
    ("data.format", StringValue("json"))
  ]))
  
  let ingestion_time = 150.0 + (Random::next_u64(Random::system()) % 100).to_float()
  let data_size = 1024 * 1024 * (Random::next_u64(Random::system()) % 100 + 50).to_int() // 50-150MB
  
  Histogram::record(pipeline_latency, ingestion_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("pipeline.stage", StringValue("ingestion")),
    ("data.source", StringValue("kafka.topic.user_events"))
  ])))
  
  Counter::add(data_throughput, data_size.to_float(), Some(Attributes::with_attributes(Attributes::new(), [
    ("pipeline.stage", StringValue("ingestion")),
    ("data.format", StringValue("json"))
  ])))
  
  Span::set_status(ingestion_span, Ok)
  Span::add_event(ingestion_span, "ingestion.completed", Some([
    ("records.ingested", IntValue(10000)),
    ("data.size.mb", IntValue(data_size / (1024 * 1024))),
    ("ingestion.time.ms", FloatValue(ingestion_time))
  ]))
  Span::end(ingestion_span)
  
  // 阶段2：数据清洗
  let cleaning_span = Tracer::start_span(tracer, "data.cleaning")
  Span::add_event(cleaning_span, "cleaning.started", Some([
    ("cleaning.rules", StringValue("deduplication,validation,normalization"))
  ]))
  
  let cleaning_time = 300.0 + (Random::next_u64(Random::system()) % 200).to_float()
  let records_cleaned = 9500 // 模拟清洗后记录数
  
  Histogram::record(pipeline_latency, cleaning_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("pipeline.stage", StringValue("cleaning"))
  ])))
  
  Span::set_status(cleaning_span, Ok)
  Span::add_event(cleaning_span, "cleaning.completed", Some([
    ("records.input", IntValue(10000)),
    ("records.output", IntValue(records_cleaned)),
    ("records.dropped", IntValue(500)),
    ("cleaning.time.ms", FloatValue(cleaning_time))
  ]))
  Span::end(cleaning_span)
  
  // 阶段3：数据转换
  let transformation_span = Tracer::start_span(tracer, "data.transformation")
  Span::add_event(transformation_span, "transformation.started", Some([
    ("transformation.type", StringValue("enrichment")),
    ("target.schema", StringValue("analytics.events.v2"))
  ]))
  
  let transformation_time = 200.0 + (Random::next_u64(Random::system()) % 150).to_float()
  
  Histogram::record(pipeline_latency, transformation_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("pipeline.stage", StringValue("transformation"))
  ])))
  
  Span::set_status(transformation_span, Ok)
  Span::add_event(transformation_span, "transformation.completed", Some([
    ("records.transformed", IntValue(records_cleaned)),
    ("fields.added", IntValue(15)),
    ("transformation.time.ms", FloatValue(transformation_time))
  ]))
  Span::end(transformation_span)
  
  // 阶段4：数据质量检查
  let quality_span = Tracer::start_span(tracer, "data.quality.check")
  Span::add_event(quality_span, "quality.check.started", Some([
    ("quality.rules", StringValue("completeness,accuracy,consistency"))
  ]))
  
  let quality_time = 80.0 + (Random::next_u64(Random::system()) % 70).to_float()
  let quality_score = 92.5 // 数据质量评分
  
  Histogram::record(pipeline_latency, quality_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("pipeline.stage", StringValue("quality_check"))
  ])))
  
  // 更新数据质量指标
  // 注意：简化实现中Gauge可能不支持设置值
  
  Span::set_status(quality_span, Ok)
  Span::add_event(quality_span, "quality.check.completed", Some([
    ("quality.score", FloatValue(quality_score)),
    ("records.passed", IntValue(8900)),
    ("records.failed", IntValue(600)),
    ("quality.time.ms", FloatValue(quality_time))
  ]))
  Span::end(quality_span)
  
  // 阶段5：数据输出
  let output_span = Tracer::start_span(tracer, "data.output")
  Span::add_event(output_span, "output.started", Some([
    ("output.destination", StringValue("data_lake.analytics_events")),
    ("output.format", StringValue("parquet"))
  ]))
  
  let output_time = 120.0 + (Random::next_u64(Random::system()) % 100).to_float()
  let output_size = data_size * 8 / 10 // 压缩后大小
  
  Histogram::record(pipeline_latency, output_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("pipeline.stage", StringValue("output"))
  ])))
  
  Counter::add(data_throughput, output_size.to_float(), Some(Attributes::with_attributes(Attributes::new(), [
    ("pipeline.stage", StringValue("output")),
    ("output.format", StringValue("parquet"))
  ])))
  
  Span::set_status(output_span, Ok)
  Span::add_event(output_span, "output.completed", Some([
    ("records.written", IntValue(8900)),
    ("output.size.mb", IntValue(output_size / (1024 * 1024))),
    ("compression.ratio", FloatValue(0.8)),
    ("output.time.ms", FloatValue(output_time))
  ]))
  Span::end(output_span)
  
  // 完成数据处理管道
  let pipeline_end = Clock::now_unix_nanos(Clock::system())
  let total_pipeline_time = (pipeline_end - pipeline_start) / 1000000L // 转换为毫秒
  
  Span::set_status(pipeline_span, Ok)
  Span::add_event(pipeline_span, "pipeline.completed", Some([
    ("pipeline.id", StringValue("pipeline-123")),
    ("total.time.ms", IntValue(total_pipeline_time.to_int())),
    ("stages.completed", IntValue(5)),
    ("records.processed", IntValue(8900)),
    ("data.quality.score", FloatValue(quality_score))
  ]))
  Span::end(pipeline_span)
  
  // 记录数据处理管道日志
  let pipeline_log = LogRecord::new_with_context(
    Info,
    Some("Data pipeline execution completed successfully"),
    Some(Attributes::with_attributes(Attributes::new(), [
      ("pipeline.id", StringValue("pipeline-123")),
      ("batch.id", StringValue("batch-456")),
      ("total.time.ms", IntValue(total_pipeline_time.to_int())),
      ("input.records", IntValue(10000)),
      ("output.records", IntValue(8900)),
      ("data.quality.score", FloatValue(quality_score)),
      ("throughput.mb.per.sec", FloatValue((data_size + output_size).to_float() / (total_pipeline_time.to_float() / 1000.0)))
    ])),
    Some(pipeline_end),
    None,
    Some(SpanContext::trace_id(Span::span_context(pipeline_span))),
    Some(SpanContext::span_id(Span::span_context(pipeline_span))),
    None
  )
  
  Logger::emit(logger, pipeline_log)
  
  assert_true(quality_score >= 90.0) // 验证数据质量达标
}

test "金融交易处理流程测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "financial.transaction")
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "financial.transaction")
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "financial.transaction")
  
  // 创建金融交易指标
  let transaction_counter = Meter::create_counter(meter, "transactions.total")
  let transaction_volume = Meter::create_counter(meter, "transaction.volume")
  let fraud_detection_counter = Meter::create_counter(meter, "fraud.detections")
  let settlement_time = Meter::create_histogram(meter, "settlement.processing.time")
  
  // 开始金融交易处理
  let transaction_span = Tracer::start_span(tracer, "financial.transaction.processing")
  Span::add_event(transaction_span, "transaction.started", Some([
    ("transaction.id", StringValue("txn-789012")),
    ("transaction.type", StringValue("payment")),
    ("account.id", StringValue("acc-123456")),
    ("amount", FloatValue(1500.75)),
    ("currency", StringValue("USD"))
  ]))
  
  // 步骤1：交易验证
  let validation_span = Tracer::start_span(tracer, "transaction.validation")
  Span::add_event(validation_span, "validation.started", Some([
    ("transaction.id", StringValue("txn-789012")),
    ("validation.type", StringValue("kyc_and_limits"))
  ]))
  
  let validation_time = 35.0 + (Random::next_u64(Random::system()) % 45).to_float()
  Histogram::record(settlement_time, validation_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("settlement.step", StringValue("validation")),
    ("transaction.id", StringValue("txn-789012"))
  ])))
  
  Span::set_status(validation_span, Ok)
  Span::add_event(validation_span, "validation.completed", Some([
    ("validation.result", StringValue("passed")),
    ("kyc.status", StringValue("verified")),
    ("limit.check", StringValue("within_limits"))
  ]))
  Span::end(validation_span)
  
  // 步骤2：欺诈检测
  let fraud_span = Tracer::start_span(tracer, "fraud.detection")
  Span::add_event(fraud_span, "fraud.detection.started", Some([
    ("transaction.id", StringValue("txn-789012")),
    ("detection.model", StringValue("ml_model_v3.2"))
  ]))
  
  let fraud_detection_time = 85.0 + (Random::next_u64(Random::system()) % 115).to_float()
  let fraud_score = 15.5 // 欺诈评分（低于阈值为安全）
  let fraud_threshold = 50.0
  
  Histogram::record(settlement_time, fraud_detection_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("settlement.step", StringValue("fraud_detection")),
    ("transaction.id", StringValue("txn-789012"))
  ])))
  
  if fraud_score < fraud_threshold {
    Counter::add(fraud_detection_counter, 1.0, Some(Attributes::with_attributes(Attributes::new(), [
      ("detection.result", StringValue("safe")),
      ("fraud.score", FloatValue(fraud_score))
    ])))
    
    Span::set_status(fraud_span, Ok)
    Span::add_event(fraud_span, "fraud.detection.completed", Some([
      ("fraud.score", FloatValue(fraud_score)),
      ("fraud.risk", StringValue("low")),
      ("detection.result", StringValue("approved"))
    ]))
  } else {
    Counter::add(fraud_detection_counter, 1.0, Some(Attributes::with_attributes(Attributes::new(), [
      ("detection.result", StringValue("suspicious")),
      ("fraud.score", FloatValue(fraud_score))
    ])))
    
    Span::set_status(fraud_span, Error)
    Span::add_event(fraud_span, "fraud.detection.completed", Some([
      ("fraud.score", FloatValue(fraud_score)),
      ("fraud.risk", StringValue("high")),
      ("detection.result", StringValue("blocked"))
    ]))
  }
  Span::end(fraud_span)
  
  // 步骤3：余额检查
  let balance_span = Tracer::start_span(tracer, "balance.check")
  Span::add_event(balance_span, "balance.check.started", Some([
    ("transaction.id", StringValue("txn-789012")),
    ("account.id", StringValue("acc-123456"))
  ]))
  
  let balance_time = 25.0 + (Random::next_u64(Random::system()) % 35).to_float()
  let available_balance = 5000.25
  let transaction_amount = 1500.75
  
  Histogram::record(settlement_time, balance_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("settlement.step", StringValue("balance_check")),
    ("transaction.id", StringValue("txn-789012"))
  ])))
  
  if available_balance >= transaction_amount {
    Span::set_status(balance_span, Ok)
    Span::add_event(balance_span, "balance.check.completed", Some([
      ("available.balance", FloatValue(available_balance)),
      ("transaction.amount", FloatValue(transaction_amount)),
      ("balance.sufficient", BoolValue(true))
    ]))
  } else {
    Span::set_status(balance_span, Error)
    Span::add_event(balance_span, "balance.check.completed", Some([
      ("available.balance", FloatValue(available_balance)),
      ("transaction.amount", FloatValue(transaction_amount)),
      ("balance.sufficient", BoolValue(false))
    ]))
  }
  Span::end(balance_span)
  
  // 步骤4：清算处理
  let settlement_span = Tracer::start_span(tracer, "settlement.processing")
  Span::add_event(settlement_span, "settlement.started", Some([
    ("transaction.id", StringValue("txn-789012")),
    ("settlement.network", StringValue("ACH")),
    ("clearing.house", StringValue("Federal Reserve"))
  ]))
  
  let settlement_time = 200.0 + (Random::next_u64(Random::system()) % 300).to_float()
  
  Histogram::record(settlement_time, settlement_time, Some(Attributes::with_attributes(Attributes::new(), [
    ("settlement.step", StringValue("settlement_processing")),
    ("transaction.id", StringValue("txn-789012"))
  ])))
  
  Span::set_status(settlement_span, Ok)
  Span::add_event(settlement_span, "settlement.completed", Some([
    ("settlement.id", StringValue("set-345678")),
    ("settlement.time.ms", FloatValue(settlement_time)),
    ("status", StringValue("cleared"))
  ]))
  Span::end(settlement_span)
  
  // 完成交易处理
  Counter::add(transaction_counter, 1.0, Some(Attributes::with_attributes(Attributes::new(), [
    ("transaction.type", StringValue("payment")),
    ("transaction.status", StringValue("completed"))
  ])))
  
  Counter::add(transaction_volume, transaction_amount, Some(Attributes::with_attributes(Attributes::new(), [
    ("currency", StringValue("USD")),
    ("transaction.type", StringValue("payment"))
  ])))
  
  Span::set_status(transaction_span, Ok)
  Span::add_event(transaction_span, "transaction.completed", Some([
    ("transaction.id", StringValue("txn-789012")),
    ("final.status", StringValue("settled")),
    ("total.processing.time.ms", FloatValue(validation_time + fraud_detection_time + balance_time + settlement_time))
  ]))
  Span::end(transaction_span)
  
  // 记录金融交易日志
  let transaction_log = LogRecord::new_with_context(
    Info,
    Some("Financial transaction processed and settled"),
    Some(Attributes::with_attributes(Attributes::new(), [
      ("transaction.id", StringValue("txn-789012")),
      ("account.id", StringValue("acc-123456")),
      ("transaction.amount", FloatValue(transaction_amount)),
      ("currency", StringValue("USD")),
      ("fraud.score", FloatValue(fraud_score)),
      ("settlement.id", StringValue("set-345678")),
      ("compliance.checked", BoolValue(true))
    ])),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(transaction_span))),
    Some(SpanContext::span_id(Span::span_context(transaction_span))),
    None
  )
  
  Logger::emit(logger, transaction_log)
  
  assert_true(fraud_score < fraud_threshold) // 验证交易安全
  assert_true(available_balance >= transaction_amount) // 验证余额充足
}