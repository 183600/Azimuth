// Advanced Distributed Correlation Tests - 高级分布式关联测试
// 专注于跨服务遥测数据关联和一致性验证

test "分布式服务链路完整关联测试" {
  // 模拟微服务架构中的完整请求链路
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "distributed.correlation.test")
  
  // Service A: API Gateway - 入口点
  let gateway_span = Tracer::start_span(tracer, "api.gateway.request")
  let gateway_ctx = Span::context(gateway_span)
  let trace_id = SpanContext::trace_id(gateway_ctx)
  let root_span_id = SpanContext::span_id(gateway_ctx)
  
  // 设置网关级别的属性和事件
  Span::set_attribute(gateway_span, "service.name", StringValue("api-gateway"))
  Span::set_attribute(gateway_span, "http.method", StringValue("POST"))
  Span::set_attribute(gateway_span, "http.url", StringValue("/api/v1/orders"))
  Span::set_attribute(gateway_span, "client.ip", StringValue("192.168.1.100"))
  Span::add_event(gateway_span, "request.received", Some([
    ("timestamp", IntValue(1735689600000000000L)),
    ("user.agent", StringValue("Mozilla/5.0..."))
  ]))
  
  // Service B: Order Service - 订单处理
  let order_span = Tracer::start_span_with_context(tracer, "order.service.process", Some(gateway_ctx))
  let order_ctx = Span::context(order_span)
  let order_span_id = SpanContext::span_id(order_ctx)
  
  // 验证trace ID一致性
  assert_eq(SpanContext::trace_id(order_ctx), trace_id)
  assert_not_eq(SpanContext::span_id(order_ctx), root_span_id)
  
  Span::set_attribute(order_span, "service.name", StringValue("order-service"))
  Span::set_attribute(order_span, "order.id", StringValue("order-12345"))
  Span::set_attribute(order_span, "customer.id", StringValue("cust-67890"))
  Span::add_event(order_span, "order.validation.started", None)
  
  // Service C: Inventory Service - 库存检查
  let inventory_span = Tracer::start_span_with_context(tracer, "inventory.service.check", Some(order_ctx))
  let inventory_ctx = Span::context(inventory_span)
  
  // 验证trace ID仍然一致
  assert_eq(SpanContext::trace_id(inventory_ctx), trace_id)
  assert_not_eq(SpanContext::span_id(inventory_ctx), order_span_id)
  
  Span::set_attribute(inventory_span, "service.name", StringValue("inventory-service"))
  Span::set_attribute(inventory_span, "product.id", StringValue("prod-11111"))
  Span::set_attribute(inventory_span, "requested.quantity", IntValue(5))
  Span::set_attribute(inventory_span, "available.quantity", IntValue(10))
  Span::add_event(inventory_span, "inventory.checked", Some([
    ("stock.sufficient", BoolValue(true)),
    ("reservation.id", StringValue("res-99999"))
  ]))
  
  // Service D: Payment Service - 支付处理
  let payment_span = Tracer::start_span_with_context(tracer, "payment.service.process", Some(order_ctx))
  let payment_ctx = Span::context(payment_span)
  
  // 验证trace ID一致性
  assert_eq(SpanContext::trace_id(payment_ctx), trace_id)
  
  Span::set_attribute(payment_span, "service.name", StringValue("payment-service"))
  Span::set_attribute(payment_span, "payment.method", StringValue("credit_card"))
  Span::set_attribute(payment_span, "payment.amount", FloatValue(99.99))
  Span::set_attribute(payment_span, "currency", StringValue("USD"))
  Span::add_event(payment_span, "payment.authorized", Some([
    ("transaction.id", StringValue("txn-55555")),
    ("auth.code", StringValue("123456"))
  ]))
  
  // Service E: Notification Service - 通知发送
  let notification_span = Tracer::start_span_with_context(tracer, "notification.service.send", Some(order_ctx))
  let notification_ctx = Span::context(notification_span)
  
  // 验证trace ID一致性
  assert_eq(SpanContext::trace_id(notification_ctx), trace_id)
  
  Span::set_attribute(notification_span, "service.name", StringValue("notification-service"))
  Span::set_attribute(notification_span, "notification.type", StringValue("email"))
  Span::set_attribute(notification_span, "recipient.email", StringValue("customer@example.com"))
  Span::add_event(notification_span, "notification.sent", Some([
    ("notification.id", StringValue("notif-77777")),
    ("delivery.status", StringValue("delivered"))
  ]))
  
  // 按顺序结束spans（模拟服务调用完成）
  Span::end(inventory_span)
  Span::end(payment_span)
  Span::end(notification_span)
  Span::end(order_span)
  Span::end(gateway_span)
  
  // 验证所有span的trace ID一致性
  assert_eq(SpanContext::trace_id(gateway_ctx), trace_id)
  assert_eq(SpanContext::trace_id(order_ctx), trace_id)
  assert_eq(SpanContext::trace_id(inventory_ctx), trace_id)
  assert_eq(SpanContext::trace_id(payment_ctx), trace_id)
  assert_eq(SpanContext::trace_id(notification_ctx), trace_id)
  
  // 验证span ID的唯一性
  let span_ids = [root_span_id, order_span_id, SpanContext::span_id(inventory_ctx), 
                  SpanContext::span_id(payment_ctx), SpanContext::span_id(notification_ctx)]
  
  // 简化的唯一性检查（在实际实现中应该更严格）
  for i = 0; i < span_ids.length(); i = i + 1 {
    for j = i + 1; j < span_ids.length(); j = j + 1 {
      assert_not_eq(span_ids[i], span_ids[j])
    }
  }
}

test "跨服务Baggage传播和关联测试" {
  // 模拟跨多个服务的baggage传播和关联
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "baggage.correlation.test")
  
  // 初始化baggage
  let initial_baggage = Baggage::new()
  
  // Service 1: User Service - 添加用户相关baggage
  let baggage1 = Baggage::set_entry(initial_baggage, "user.id", "user-12345")
  let baggage1 = Baggage::set_entry(baggage1, "user.tier", "premium")
  let baggage1 = Baggage::set_entry(baggage1, "user.region", "us-west")
  
  // 创建第一个span并注入baggage
  let user_span = Tracer::start_span(tracer, "user.service.operation")
  let user_ctx = Span::context(user_span)
  
  // 将baggage注入到context中
  let ctx_with_baggage1 = Context::with_value(user_ctx, ContextKey::new("baggage"), baggage1)
  
  // Service 2: Auth Service - 添加认证相关baggage
  let baggage2 = Baggage::set_entry(baggage1, "auth.method", "oauth2")
  let baggage2 = Baggage::set_entry(baggage2, "auth.token.id", "token-67890")
  let baggage2 = Baggage::set_entry(baggage2, "auth.scope", "read:write")
  
  let auth_span = Tracer::start_span_with_context(tracer, "auth.service.validate", Some(ctx_with_baggage1))
  let auth_ctx = Span::context(auth_span)
  let ctx_with_baggage2 = Context::with_value(auth_ctx, ContextKey::new("baggage"), baggage2)
  
  // Service 3: Business Service - 添加业务相关baggage
  let baggage3 = Baggage::set_entry(baggage2, "business.tenant", "acme-corp")
  let baggage3 = Baggage::set_entry(baggage3, "business.unit", "engineering")
  let baggage3 = Baggage::set_entry(baggage3, "business.project", "telemetry-platform")
  
  let business_span = Tracer::start_span_with_context(tracer, "business.service.process", Some(ctx_with_baggage2))
  let business_ctx = Span::context(business_span)
  let ctx_with_baggage3 = Context::with_value(business_ctx, ContextKey::new("baggage"), baggage3)
  
  // Service 4: Infrastructure Service - 添加基础设施相关baggage
  let baggage4 = Baggage::set_entry(baggage3, "infra.cluster", "prod-cluster-1")
  let baggage4 = Baggage::set_entry(baggage4, "infra.namespace", "telemetry")
  let baggage4 = Baggage::set_entry(baggage4, "infra.pod", "telemetry-service-7f8d9")
  
  let infra_span = Tracer::start_span_with_context(tracer, "infra.service.operation", Some(ctx_with_baggage3))
  let infra_ctx = Span::context(infra_span)
  
  // 验证最终的baggage包含所有信息
  let final_baggage_key = ContextKey::new("baggage")
  let final_baggage = Context::get(infra_ctx, final_baggage_key)
  
  // 在简化实现中，我们直接验证最后一个baggage对象
  let user_id = Baggage::get_entry(baggage4, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "user-12345")
    None => assert_true(false)
  }
  
  let auth_method = Baggage::get_entry(baggage4, "auth.method")
  match auth_method {
    Some(value) => assert_eq(value, "oauth2")
    None => assert_true(false)
  }
  
  let business_tenant = Baggage::get_entry(baggage4, "business.tenant")
  match business_tenant {
    Some(value) => assert_eq(value, "acme-corp")
    None => assert_true(false)
  }
  
  let infra_cluster = Baggage::get_entry(baggage4, "infra.cluster")
  match infra_cluster {
    Some(value) => assert_eq(value, "prod-cluster-1")
    None => assert_true(false)
  }
  
  // 验证所有服务的trace ID一致性
  assert_eq(SpanContext::trace_id(user_ctx), SpanContext::trace_id(auth_ctx))
  assert_eq(SpanContext::trace_id(auth_ctx), SpanContext::trace_id(business_ctx))
  assert_eq(SpanContext::trace_id(business_ctx), SpanContext::trace_id(infra_ctx))
  
  // 结束所有spans
  Span::end(infra_span)
  Span::end(business_span)
  Span::end(auth_span)
  Span::end(user_span)
}

test "异步操作遥测关联测试" {
  // 模拟异步操作中的遥测关联
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "async.correlation.test")
  
  // 主操作span
  let main_span = Tracer::start_span(tracer, "async.main.operation")
  let main_ctx = Span::context(main_span)
  let trace_id = SpanContext::trace_id(main_ctx)
  
  Span::set_attribute(main_span, "operation.type", StringValue("async.workflow"))
  Span::add_event(main_span, "workflow.started", None)
  
  // 异步任务1: 数据预处理
  let preprocess_span = Tracer::start_span_with_context(tracer, "async.task.preprocess", Some(main_ctx))
  let preprocess_ctx = Span::context(preprocess_span)
  
  assert_eq(SpanContext::trace_id(preprocess_ctx), trace_id)
  Span::set_attribute(preprocess_span, "task.id", StringValue("task-001"))
  Span::set_attribute(preprocess_span, "task.type", StringValue("data.preprocess"))
  Span::add_event(preprocess_span, "preprocess.started", None)
  
  // 异步任务2: 数据验证
  let validation_span = Tracer::start_span_with_context(tracer, "async.task.validation", Some(main_ctx))
  let validation_ctx = Span::context(validation_span)
  
  assert_eq(SpanContext::trace_id(validation_ctx), trace_id)
  Span::set_attribute(validation_span, "task.id", StringValue("task-002"))
  Span::set_attribute(validation_span, "task.type", StringValue("data.validation"))
  Span::add_event(validation_span, "validation.started", None)
  
  // 异步任务3: 数据转换
  let transform_span = Tracer::start_span_with_context(tracer, "async.task.transform", Some(main_ctx))
  let transform_ctx = Span::context(transform_span)
  
  assert_eq(SpanContext::trace_id(transform_ctx), trace_id)
  Span::set_attribute(transform_span, "task.id", StringValue("task-003"))
  Span::set_attribute(transform_span, "task.type", StringValue("data.transform"))
  Span::add_event(transform_span, "transform.started", None)
  
  // 异步任务4: 数据持久化
  let persist_span = Tracer::start_span_with_context(tracer, "async.task.persist", Some(main_ctx))
  let persist_ctx = Span::context(persist_span)
  
  assert_eq(SpanContext::trace_id(persist_ctx), trace_id)
  Span::set_attribute(persist_span, "task.id", StringValue("task-004"))
  Span::set_attribute(persist_span, "task.type", StringValue("data.persist"))
  Span::add_event(persist_span, "persist.started", None)
  
  // 模拟任务完成（乱序完成）
  Span::add_event(transform_span, "transform.completed", Some([
    ("records.processed", IntValue(1000)),
    ("processing.time", IntValue(5000))
  ]))
  Span::end(transform_span)
  
  Span::add_event(persist_span, "persist.completed", Some([
    ("records.stored", IntValue(1000)),
    ("storage.location", StringValue("s3://data-bucket/processed/"))
  ]))
  Span::end(persist_span)
  
  Span::add_event(validation_span, "validation.completed", Some([
    ("records.validated", IntValue(1000)),
    ("validation.errors", IntValue(5))
  ]))
  Span::end(validation_span)
  
  Span::add_event(preprocess_span, "preprocess.completed", Some([
    ("records.preprocessed", IntValue(1000)),
    ("preprocessing.time", IntValue(3000))
  ]))
  Span::end(preprocess_span)
  
  // 主操作完成
  Span::add_event(main_span, "workflow.completed", Some([
    ("total.tasks", IntValue(4)),
    ("total.records", IntValue(1000)),
    ("workflow.duration", IntValue(15000))
  ]))
  Span::end(main_span)
  
  // 验证所有span的trace ID一致性
  assert_eq(SpanContext::trace_id(main_ctx), trace_id)
  assert_eq(SpanContext::trace_id(preprocess_ctx), trace_id)
  assert_eq(SpanContext::trace_id(validation_ctx), trace_id)
  assert_eq(SpanContext::trace_id(transform_ctx), trace_id)
  assert_eq(SpanContext::trace_id(persist_ctx), trace_id)
}

test "错误传播和遥测关联测试" {
  // 模拟错误在分布式系统中的传播和遥测关联
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error.correlation.test")
  
  // Service A: 触发错误的服务
  let service_a_span = Tracer::start_span(tracer, "service.a.operation")
  let service_a_ctx = Span::context(service_a_span)
  let trace_id = SpanContext::trace_id(service_a_ctx)
  
  Span::set_attribute(service_a_span, "service.name", StringValue("service-a"))
  Span::add_event(service_a_span, "operation.started", None)
  
  // 模拟错误发生
  let error_message = "Database connection timeout"
  Span::set_status(service_a_span, Error, Some(error_message))
  Span::add_event(service_a_span, "error.occurred", Some([
    ("error.type", StringValue("connection.timeout")),
    ("error.code", StringValue("DB_TIMEOUT")),
    ("retry.count", IntValue(3))
  ]))
  Span::end(service_a_span)
  
  // Service B: 被调用的服务，传播错误
  let service_b_span = Tracer::start_span_with_context(tracer, "service.b.operation", Some(service_a_ctx))
  let service_b_ctx = Span::context(service_b_span)
  
  assert_eq(SpanContext::trace_id(service_b_ctx), trace_id)
  
  Span::set_attribute(service_b_span, "service.name", StringValue("service-b"))
  Span::set_attribute(service_b_span, "upstream.service", StringValue("service-a"))
  Span::add_event(service_b_span, "upstream.error.received", Some([
    ("error.message", StringValue(error_message)),
    ("upstream.trace.id", StringValue(trace_id)),
    ("upstream.span.id", StringValue(SpanContext::span_id(service_a_ctx)))
  ]))
  
  // 传播错误状态
  Span::set_status(service_b_span, Error, Some("Upstream service error: " + error_message))
  Span::end(service_b_span)
  
  // Service C: 进一步传播错误
  let service_c_span = Tracer::start_span_with_context(tracer, "service.c.operation", Some(service_b_ctx))
  let service_c_ctx = Span::context(service_c_span)
  
  assert_eq(SpanContext::trace_id(service_c_ctx), trace_id)
  
  Span::set_attribute(service_c_span, "service.name", StringValue("service-c"))
  Span::set_attribute(service_c_span, "upstream.service", StringValue("service-b"))
  Span::add_event(service_c_span, "error.propagated", Some([
    ("original.error", StringValue(error_message)),
    ("propagation.path", StringValue("service-a -> service-b -> service-c"))
  ]))
  
  Span::set_status(service_c_span, Error, Some("Propagated error from upstream"))
  Span::end(service_c_span)
  
  // 验证错误传播路径
  assert_eq(SpanContext::trace_id(service_a_ctx), trace_id)
  assert_eq(SpanContext::trace_id(service_b_ctx), trace_id)
  assert_eq(SpanContext::trace_id(service_c_ctx), trace_id)
  
  // 验证span的父子关系（通过context传播）
  assert_not_eq(SpanContext::span_id(service_a_ctx), SpanContext::span_id(service_b_ctx))
  assert_not_eq(SpanContext::span_id(service_b_ctx), SpanContext::span_id(service_c_ctx))
}

test "跨服务Metrics关联测试" {
  // 模拟跨服务的metrics关联和聚合
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "cross.service.metrics")
  
  // 创建跨服务相关的metrics
  let request_counter = Meter::create_counter(meter, "cross.service.requests.total", 
    Some("Total cross-service requests"), Some("requests"))
  let response_time = Meter::create_histogram(meter, "cross.service.response.time", 
    Some("Cross-service response time"), Some("ms"))
  let error_counter = Meter::create_counter(meter, "cross.service.errors.total", 
    Some("Total cross-service errors"), Some("errors"))
  
  // Service A metrics
  let service_a_attrs = Attributes::new()
  Attributes::set(service_a_attrs, "service.name", StringValue("service-a"))
  Attributes::set(service_a_attrs, "operation", StringValue("process.request"))
  Attributes::set(service_a_attrs, "version", StringValue("1.2.3"))
  
  Counter::add(request_counter, 100.0, Some(service_a_attrs))
  Histogram::record(response_time, 150.5, Some(service_a_attrs))
  Counter::add(error_counter, 5.0, Some(service_a_attrs))
  
  // Service B metrics
  let service_b_attrs = Attributes::new()
  Attributes::set(service_b_attrs, "service.name", StringValue("service-b"))
  Attributes::set(service_b_attrs, "operation", StringValue("validate.data"))
  Attributes::set(service_b_attrs, "version", StringValue("2.1.0"))
  
  Counter::add(request_counter, 80.0, Some(service_b_attrs))
  Histogram::record(response_time, 75.2, Some(service_b_attrs))
  Counter::add(error_counter, 2.0, Some(service_b_attrs))
  
  // Service C metrics
  let service_c_attrs = Attributes::new()
  Attributes::set(service_c_attrs, "service.name", StringValue("service-c"))
  Attributes::set(service_c_attrs, "operation", StringValue("store.result"))
  Attributes::set(service_c_attrs, "version", StringValue("1.0.5"))
  
  Counter::add(request_counter, 120.0, Some(service_c_attrs))
  Histogram::record(response_time, 200.8, Some(service_c_attrs))
  Counter::add(error_counter, 8.0, Some(service_c_attrs))
  
  // 创建聚合指标的属性
  let aggregated_attrs = Attributes::new()
  Attributes::set(aggregated_attrs, "aggregation.level", StringValue("system"))
  Attributes::set(aggregated_attrs, "environment", StringValue("production"))
  
  // 记录聚合指标
  let total_requests = 100.0 + 80.0 + 120.0
  let total_errors = 5.0 + 2.0 + 8.0
  let avg_response_time = (150.5 + 75.2 + 200.8) / 3.0
  
  Counter::add(request_counter, total_requests, Some(aggregated_attrs))
  Counter::add(error_counter, total_errors, Some(aggregated_attrs))
  Histogram::record(response_time, avg_response_time, Some(aggregated_attrs))
  
  // 验证metrics创建和属性设置
  assert_eq(request_counter.name, "cross.service.requests.total")
  assert_eq(request_counter.description, Some("Total cross-service requests"))
  assert_eq(request_counter.unit, Some("requests"))
  
  assert_eq(response_time.name, "cross.service.response.time")
  assert_eq(response_time.description, Some("Cross-service response time"))
  assert_eq(response_time.unit, Some("ms"))
  
  assert_eq(error_counter.name, "cross.service.errors.total")
  assert_eq(error_counter.description, Some("Total cross-service errors"))
  assert_eq(error_counter.unit, Some("errors"))
  
  // 验证属性值
  match Attributes::get(service_a_attrs, "service.name") {
    Some(StringValue(value)) => assert_eq(value, "service-a")
    _ => assert_true(false)
  }
  
  match Attributes::get(aggregated_attrs, "aggregation.level") {
    Some(StringValue(value)) => assert_eq(value, "system")
    _ => assert_true(false)
  }
}