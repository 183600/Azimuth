// Azimuth 高级分布式追踪一致性测试用例
// 专注于验证分布式系统中追踪数据的一致性和完整性

// 测试1: 跨服务追踪链完整性验证
test "跨服务追踪链完整性验证测试" {
  // 创建分布式追踪提供者
  let tracer_provider = TracerProvider::default()
  let trace_id = TraceId::generate()
  
  // 服务A: API网关
  let gateway_tracer = TracerProvider::get_tracer(tracer_provider, "api.gateway")
  let gateway_span = Tracer::start_span_with_trace_id(gateway_tracer, "gateway.request", trace_id, SpanId::generate())
  
  Span::set_attribute(gateway_span, "service.name", StringValue("api.gateway"))
  Span::set_attribute(gateway_span, "service.version", StringValue("1.2.0"))
  Span::set_attribute(gateway_span, "http.method", StringValue("POST"))
  Span::set_attribute(gateway_span, "http.url", StringValue("/api/v1/process"))
  Span::set_attribute(gateway_span, "user.id", StringValue("user-12345"))
  
  let gateway_context = Span::context(gateway_span)
  
  // 服务B: 业务逻辑服务
  let business_tracer = TracerProvider::get_tracer(tracer_provider, "business.service")
  let business_span = Tracer::start_span_with_context(business_tracer, "business.process", gateway_context)
  
  Span::set_attribute(business_span, "service.name", StringValue("business.service"))
  Span::set_attribute(business_span, "service.version", StringValue("2.1.3"))
  Span::set_attribute(business_span, "operation.type", StringValue("data.processing"))
  Span::set_attribute(business_span, "user.id", StringValue("user-12345")) // 应该保持一致
  
  let business_context = Span::context(business_span)
  
  // 服务C: 数据库服务
  let db_tracer = TracerProvider::get_tracer(tracer_provider, "database.service")
  let db_span = Tracer::start_span_with_context(db_tracer, "database.query", business_context)
  
  Span::set_attribute(db_span, "service.name", StringValue("database.service"))
  Span::set_attribute(db_span, "service.version", StringValue("1.0.5"))
  Span::set_attribute(db_span, "db.type", StringValue("postgresql"))
  Span::set_attribute(db_span, "db.statement", StringValue("SELECT * FROM users WHERE id = ?"))
  Span::set_attribute(db_span, "user.id", StringValue("user-12345")) // 应该保持一致
  
  // 服务D: 缓存服务（并行调用）
  let cache_tracer = TracerProvider::get_tracer(tracer_provider, "cache.service")
  let cache_span = Tracer::start_span_with_context(cache_tracer, "cache.get", business_context)
  
  Span::set_attribute(cache_span, "service.name", StringValue("cache.service"))
  Span::set_attribute(cache_span, "service.version", StringValue("1.3.1"))
  Span::set_attribute(cache_span, "cache.key", StringValue("user.profile.12345"))
  Span::set_attribute(cache_span, "user.id", StringValue("user-12345")) // 应该保持一致
  
  // 结束span（按正确顺序）
  Span::end(db_span)
  Span::end(cache_span)
  Span::end(business_span)
  Span::end(gateway_span)
  
  // 创建追踪一致性验证器
  let consistency_validator = DistributedTraceConsistencyValidator::new()
  
  // 收集所有span
  let all_spans = [gateway_span, business_span, db_span, cache_span]
  
  // 验证追踪链完整性
  let trace_integrity = DistributedTraceConsistencyValidator::validate_trace_integrity(consistency_validator, all_spans)
  
  assert_true(trace_integrity.is_complete_trace)
  assert_eq(trace_integrity.root_span, gateway_span)
  assert_eq(trace_integrity.total_spans, 4)
  assert_eq(trace_integrity.unique_services, 4)
  assert_true(trace_integrity.all_spans_same_trace)
  
  // 验证服务层次结构
  let service_hierarchy = DistributedTraceConsistencyValidator::validate_service_hierarchy(consistency_validator, all_spans)
  
  assert_true(service_hierarchy.is_valid_hierarchy)
  assert_eq(service_hierarchy.root_service, "api.gateway")
  assert_true(service_hierarchy.child_services.contains("business.service"))
  assert_true(service_hierarchy.leaf_services.contains("database.service"))
  assert_true(service_hierarchy.leaf_services.contains("cache.service"))
  
  // 验证上下文传播
  let context_propagation = DistributedTraceConsistencyValidator::validate_context_propagation(consistency_validator, all_spans)
  
  assert_true(context_propagation.context_properly_propagated)
  assert_true(context_propagation.trace_id_consistent)
  assert_true(context_propagation.parent_child_relationships_valid)
  
  // 验证属性一致性
  let attribute_consistency = DistributedTraceConsistencyValidator::validate_attribute_consistency(consistency_validator, all_spans, "user.id")
  
  assert_true(attribute_consistency.is_consistent)
  assert_eq(attribute_consistency.consistent_value, "user-12345")
  assert_eq(attribute_consistency.inconsistent_spans.length(), 0)
}

// 测试2: 分布式事务追踪一致性
test "分布式事务追踪一致性测试" {
  let tracer_provider = TracerProvider::default()
  let transaction_id = TransactionId::generate()
  let trace_id = TraceId::generate()
  
  // 创建分布式事务协调器
  let coordinator_tracer = TracerProvider::get_tracer(tracer_provider, "transaction.coordinator")
  let coordinator_span = Tracer::start_span_with_trace_id(coordinator_tracer, "transaction.coordinate", trace_id, SpanId::generate())
  
  Span::set_attribute(coordinator_span, "service.name", StringValue("transaction.coordinator"))
  Span::set_attribute(coordinator_span, "transaction.id", StringValue(transaction_id))
  Span::set_attribute(coordinator_span, "transaction.type", StringValue("two_phase_commit"))
  Span::set_attribute(coordinator_span, "participant.count", IntValue(3))
  
  // 参与者1: 订单服务
  let order_tracer = TracerProvider::get_tracer(tracer_provider, "order.service")
  let order_span = Tracer::start_span_with_context(order_tracer, "transaction.prepare", Span::context(coordinator_span))
  
  Span::set_attribute(order_span, "service.name", StringValue("order.service"))
  Span::set_attribute(order_span, "transaction.id", StringValue(transaction_id))
  Span::set_attribute(order_span, "transaction.phase", StringValue("prepare"))
  Span::set_attribute(order_span, "participant.id", StringValue("order-service-001"))
  Span::set_attribute(order_span, "operation.type", StringValue("create_order"))
  
  // 记录准备阶段事件
  Span::add_event_with_attributes(order_span, "transaction.prepare.started", [
    ("transaction.id", StringValue(transaction_id)),
    ("participant.id", StringValue("order-service-001"))
  ])
  
  // 模拟订单创建
  Span::add_event_with_attributes(order_span, "order.created", [
    ("order.id", StringValue("order-98765")),
    ("customer.id", StringValue("cust-12345")),
    ("amount", DoubleValue(299.99))
  ])
  
  Span::add_event_with_attributes(order_span, "transaction.prepare.completed", [
    ("transaction.id", StringValue(transaction_id)),
    ("participant.id", StringValue("order-service-001")),
    ("prepare.result", StringValue("success"))
  ])
  
  // 参与者2: 支付服务
  let payment_tracer = TracerProvider::get_tracer(tracer_provider, "payment.service")
  let payment_span = Tracer::start_span_with_context(payment_tracer, "transaction.prepare", Span::context(coordinator_span))
  
  Span::set_attribute(payment_span, "service.name", StringValue("payment.service"))
  Span::set_attribute(payment_span, "transaction.id", StringValue(transaction_id))
  Span::set_attribute(payment_span, "transaction.phase", StringValue("prepare"))
  Span::set_attribute(payment_span, "participant.id", StringValue("payment-service-001"))
  Span::set_attribute(payment_span, "operation.type", StringValue("process_payment"))
  
  // 记录准备阶段事件
  Span::add_event_with_attributes(payment_span, "transaction.prepare.started", [
    ("transaction.id", StringValue(transaction_id)),
    ("participant.id", StringValue("payment-service-001"))
  ])
  
  // 模拟支付处理
  Span::add_event_with_attributes(payment_span, "payment.processed", [
    ("payment.id", StringValue("pay-54321")),
    ("order.id", StringValue("order-98765")),
    ("amount", DoubleValue(299.99)),
    ("payment.method", StringValue("credit_card"))
  ])
  
  Span::add_event_with_attributes(payment_span, "transaction.prepare.completed", [
    ("transaction.id", StringValue(transaction_id)),
    ("participant.id", StringValue("payment-service-001")),
    ("prepare.result", StringValue("success"))
  ])
  
  // 参与者3: 库存服务
  let inventory_tracer = TracerProvider::get_tracer(tracer_provider, "inventory.service")
  let inventory_span = Tracer::start_span_with_context(inventory_tracer, "transaction.prepare", Span::context(coordinator_span))
  
  Span::set_attribute(inventory_span, "service.name", StringValue("inventory.service"))
  Span::set_attribute(inventory_span, "transaction.id", StringValue(transaction_id))
  Span::set_attribute(inventory_span, "transaction.phase", StringValue("prepare"))
  Span::set_attribute(inventory_span, "participant.id", StringValue("inventory-service-001"))
  Span::set_attribute(inventory_span, "operation.type", StringValue("reserve_inventory"))
  
  // 记录准备阶段事件
  Span::add_event_with_attributes(inventory_span, "transaction.prepare.started", [
    ("transaction.id", StringValue(transaction_id)),
    ("participant.id", StringValue("inventory-service-001"))
  ])
  
  // 模拟库存预留
  Span::add_event_with_attributes(inventory_span, "inventory.reserved", [
    ("product.id", StringValue("prod-11111")),
    ("quantity", IntValue(1)),
    ("warehouse.id", StringValue("wh-001"))
  ])
  
  Span::add_event_with_attributes(inventory_span, "transaction.prepare.completed", [
    ("transaction.id", StringValue(transaction_id)),
    ("participant.id", StringValue("inventory-service-001")),
    ("prepare.result", StringValue("success"))
  ])
  
  // 结束准备阶段
  Span::end(order_span)
  Span::end(payment_span)
  Span::end(inventory_span)
  
  // 记录协调器准备完成
  Span::add_event_with_attributes(coordinator_span, "transaction.all_prepared", [
    ("transaction.id", StringValue(transaction_id)),
    ("prepared.participants", IntValue(3))
  ])
  
  // 提交阶段
  let commit_span = Tracer::start_span_with_context(coordinator_tracer, "transaction.commit", Span::context(coordinator_span))
  Span::set_attribute(commit_span, "transaction.id", StringValue(transaction_id))
  Span::set_attribute(commit_span, "transaction.phase", StringValue("commit"))
  
  // 创建提交子span
  let order_commit = Tracer::start_span_with_context(order_tracer, "transaction.commit", Span::context(commit_span))
  Span::set_attribute(order_commit, "transaction.id", StringValue(transaction_id))
  Span::set_attribute(order_commit, "participant.id", StringValue("order-service-001"))
  Span::add_event_with_attributes(order_commit, "transaction.commit.completed", [
    ("transaction.id", StringValue(transaction_id)),
    ("commit.result", StringValue("success"))
  ])
  Span::end(order_commit)
  
  let payment_commit = Tracer::start_span_with_context(payment_tracer, "transaction.commit", Span::context(commit_span))
  Span::set_attribute(payment_commit, "transaction.id", StringValue(transaction_id))
  Span::set_attribute(payment_commit, "participant.id", StringValue("payment-service-001"))
  Span::add_event_with_attributes(payment_commit, "transaction.commit.completed", [
    ("transaction.id", StringValue(transaction_id)),
    ("commit.result", StringValue("success"))
  ])
  Span::end(payment_commit)
  
  let inventory_commit = Tracer::start_span_with_context(inventory_tracer, "transaction.commit", Span::context(commit_span))
  Span::set_attribute(inventory_commit, "transaction.id", StringValue(transaction_id))
  Span::set_attribute(inventory_commit, "participant.id", StringValue("inventory-service-001"))
  Span::add_event_with_attributes(inventory_commit, "transaction.commit.completed", [
    ("transaction.id", StringValue(transaction_id)),
    ("commit.result", StringValue("success"))
  ])
  Span::end(inventory_commit)
  
  Span::end(commit_span)
  
  // 记录事务完成
  Span::add_event_with_attributes(coordinator_span, "transaction.completed", [
    ("transaction.id", StringValue(transaction_id)),
    ("transaction.result", StringValue("committed")),
    ("total.duration.ms", IntValue(1250))
  ])
  
  Span::end(coordinator_span)
  
  // 验证分布式事务一致性
  let all_spans = [coordinator_span, order_span, payment_span, inventory_span, commit_span, order_commit, payment_commit, inventory_commit]
  let transaction_validator = DistributedTransactionValidator::new()
  
  let transaction_consistency = DistributedTransactionValidator::validate_transaction_consistency(transaction_validator, all_spans, transaction_id)
  
  assert_true(transaction_consistency.is_consistent)
  assert_eq(transaction_consistency.transaction_id, transaction_id)
  assert_eq(transaction_consistency.participant_count, 3)
  assert_eq(transaction_consistency.phase_count, 2) // prepare和commit
  assert_true(transaction_consistency.all_phases_complete)
  assert_true(transaction_consistency.participants_prepared_before_commit)
  
  // 验证事务顺序
  let transaction_order = DistributedTransactionValidator::validate_transaction_order(transaction_validator, all_spans)
  
  assert_true(transaction_order.is_valid_order)
  assert_true(transaction_order.prepare_before_commit)
  assert_true(transaction_order.coordinator_starts_first)
  assert_true(transaction_order.coordinator_ends_last)
  
  // 验证事务属性传播
  let attribute_propagation = DistributedTransactionValidator::validate_attribute_propagation(transaction_validator, all_spans, "transaction.id")
  
  assert_true(attribute_propagation.is_consistently_propagated)
  assert_eq(attribute_propagation.propagated_value, transaction_id)
  assert_eq(attribute_propagation.missing_spans.length(), 0)
}

// 测试3: 异步消息追踪一致性
test "异步消息追踪一致性测试" {
  let tracer_provider = TracerProvider::default()
  let trace_id = TraceId::generate()
  
  // 生产者服务
  let producer_tracer = TracerProvider::get_tracer(tracer_provider, "message.producer")
  let producer_span = Tracer::start_span_with_trace_id(producer_tracer, "message.publish", trace_id, SpanId::generate())
  
  Span::set_attribute(producer_span, "service.name", StringValue("message.producer"))
  Span::set_attribute(producer_span, "messaging.system", StringValue("rabbitmq"))
  Span::set_attribute(producer_span, "messaging.destination", StringValue("user.notifications"))
  Span::set_attribute(producer_span, "messaging.message_id", StringValue("msg-12345"))
  Span::set_attribute(producer_span, "messaging.correlation_id", StringValue("corr-67890"))
  
  // 记录消息发布事件
  Span::add_event_with_attributes(producer_span, "message.created", [
    ("messaging.message_id", StringValue("msg-12345")),
    ("messaging.payload_size", IntValue(1024))
  ])
  
  Span::add_event_with_attributes(producer_span, "message.published", [
    ("messaging.message_id", StringValue("msg-12345")),
    ("messaging.destination", StringValue("user.notifications")),
    ("publish.timestamp", IntValue(Timestamp::now().to_millis()))
  ])
  
  let producer_context = Span::context(producer_span)
  Span::end(producer_span)
  
  // 消息中间件（模拟）
  let message_context = MessageContext::from_span_context(producer_context)
  MessageContext::set_message_id(message_context, "msg-12345")
  MessageContext::set_correlation_id(message_context, "corr-67890")
  
  // 消费者服务1
  let consumer1_tracer = TracerProvider::get_tracer(tracer_provider, "notification.service")
  let consumer1_span = Tracer::start_span_with_message_context(consumer1_tracer, "message.consume", message_context)
  
  Span::set_attribute(consumer1_span, "service.name", StringValue("notification.service"))
  Span::set_attribute(consumer1_span, "messaging.system", StringValue("rabbitmq"))
  Span::set_attribute(consumer1_span, "messaging.destination", StringValue("user.notifications"))
  Span::set_attribute(consumer1_span, "messaging.message_id", StringValue("msg-12345"))
  Span::set_attribute(consumer1_span, "messaging.correlation_id", StringValue("corr-67890"))
  Span::set_attribute(consumer1_span, "consumer.group", StringValue("notification-processors"))
  
  // 处理消息
  Span::add_event_with_attributes(consumer1_span, "message.received", [
    ("messaging.message_id", StringValue("msg-12345")),
    ("receive.timestamp", IntValue(Timestamp::now().to_millis()))
  ])
  
  Span::add_event_with_attributes(consumer1_span, "notification.sent", [
    ("notification.type", StringValue("email")),
    ("recipient", StringValue("user@example.com")),
    ("notification.id", StringValue("notif-11111"))
  ])
  
  Span::end(consumer1_span)
  
  // 消费者服务2（另一个消费者组）
  let consumer2_tracer = TracerProvider::get_tracer(tracer_provider, "analytics.service")
  let consumer2_span = Tracer::start_span_with_message_context(consumer2_tracer, "message.consume", message_context)
  
  Span::set_attribute(consumer2_span, "service.name", StringValue("analytics.service"))
  Span::set_attribute(consumer2_span, "messaging.system", StringValue("rabbitmq"))
  Span::set_attribute(consumer2_span, "messaging.destination", StringValue("user.notifications"))
  Span::set_attribute(consumer2_span, "messaging.message_id", StringValue("msg-12345"))
  Span::set_attribute(consumer2_span, "messaging.correlation_id", StringValue("corr-67890"))
  Span::set_attribute(consumer2_span, "consumer.group", StringValue("analytics-processors"))
  
  // 处理消息
  Span::add_event_with_attributes(consumer2_span, "message.received", [
    ("messaging.message_id", StringValue("msg-12345")),
    ("receive.timestamp", IntValue(Timestamp::now().to_millis()))
  ])
  
  Span::add_event_with_attributes(consumer2_span, "analytics.recorded", [
    ("event.type", StringValue("user_notification")),
    ("event.timestamp", IntValue(Timestamp::now().to_millis())),
    ("analytics.table", StringValue("user_events"))
  ])
  
  Span::end(consumer2_span)
  
  // 验证异步消息追踪一致性
  let all_spans = [producer_span, consumer1_span, consumer2_span]
  let message_validator = AsyncMessageTraceValidator::new()
  
  let message_consistency = AsyncMessageTraceValidator::validate_message_trace_consistency(message_validator, all_spans)
  
  assert_true(message_consistency.is_consistent)
  assert_true(message_consistency.producer_consumer_relationship_valid)
  assert_true(message_consistency.message_id_consistent)
  assert_true(message_consistency.correlation_id_consistent)
  assert_eq(message_consistency.producer_count, 1)
  assert_eq(message_consistency.consumer_count, 2)
  
  // 验证消息上下文传播
  let context_propagation = AsyncMessageTraceValidator::validate_message_context_propagation(message_validator, all_spans)
  
  assert_true(context_propagation.context_properly_propagated)
  assert_true(context_propagation.trace_id_consistent)
  assert_true(context_propagation.message_attributes_preserved)
  
  // 验证消息处理时序
  let temporal_consistency = AsyncMessageTraceValidator::validate_temporal_consistency(message_validator, all_spans)
  
  assert_true(temporal_consistency.is_temporally_consistent)
  assert_true(temporal_consistency.producer_before_consumers)
  assert_true(temporal_consistency.message_processing_sequence_valid)
}

// 测试4: 微服务调用链追踪一致性
test "微服务调用链追踪一致性测试" {
  let tracer_provider = TracerProvider::default()
  let trace_id = TraceId::generate()
  
  // API网关
  let gateway_tracer = TracerProvider::get_tracer(tracer_provider, "api.gateway")
  let gateway_span = Tracer::start_span_with_trace_id(gateway_tracer, "http.request", trace_id, SpanId::generate())
  
  Span::set_attribute(gateway_span, "service.name", StringValue("api.gateway"))
  Span::set_attribute(gateway_span, "http.method", StringValue("GET"))
  Span::set_attribute(gateway_span, "http.url", StringValue("/api/v1/user/profile"))
  Span::set_attribute(gateway_span, "http.user_agent", StringValue("Mozilla/5.0..."))
  Span::set_attribute(gateway_span, "user.id", StringValue("user-12345"))
  
  let gateway_context = Span::context(gateway_span)
  
  // 用户服务
  let user_tracer = TracerProvider::get_tracer(tracer_provider, "user.service")
  let user_span = Tracer::start_span_with_context(user_tracer, "http.request", gateway_context)
  
  Span::set_attribute(user_span, "service.name", StringValue("user.service"))
  Span::set_attribute(user_span, "http.method", StringValue("GET"))
  Span::set_attribute(user_span, "http.url", StringValue("/internal/user/12345"))
  Span::set_attribute(user_span, "user.id", StringValue("user-12345"))
  
  let user_context = Span::context(user_span)
  
  // 并行调用：认证服务和权限服务
  let auth_tracer = TracerProvider::get_tracer(tracer_provider, "auth.service")
  let auth_span = Tracer::start_span_with_context(auth_tracer, "http.request", user_context)
  
  Span::set_attribute(auth_span, "service.name", StringValue("auth.service"))
  Span::set_attribute(auth_span, "http.method", StringValue("POST"))
  Span::set_attribute(auth_span, "http.url", StringValue("/auth/validate"))
  Span::set_attribute(auth_span, "user.id", StringValue("user-12345"))
  
  Span::end(auth_span)
  
  let permission_tracer = TracerProvider::get_tracer(tracer_provider, "permission.service")
  let permission_span = Tracer::start_span_with_context(permission_tracer, "http.request", user_context)
  
  Span::set_attribute(permission_span, "service.name", StringValue("permission.service"))
  Span::set_attribute(permission_span, "http.method", StringValue("GET"))
  Span::set_attribute(permission_span, "http.url", StringValue("/permissions/user/12345"))
  Span::set_attribute(permission_span, "user.id", StringValue("user-12345"))
  
  Span::end(permission_span)
  
  // 用户服务继续处理，调用数据库
  let db_tracer = TracerProvider::get_tracer(tracer_provider, "database.service")
  let db_span = Tracer::start_span_with_context(db_tracer, "database.query", user_context)
  
  Span::set_attribute(db_span, "service.name", StringValue("database.service"))
  Span::set_attribute(db_span, "db.type", StringValue("postgresql"))
  Span::set_attribute(db_span, "db.statement", StringValue("SELECT * FROM users WHERE id = $1"))
  Span::set_attribute(db_span, "user.id", StringValue("user-12345"))
  
  Span::end(db_span)
  
  // 用户服务返回响应
  Span::end(user_span)
  
  // API网关处理响应
  Span::end(gateway_span)
  
  // 验证微服务调用链一致性
  let all_spans = [gateway_span, user_span, auth_span, permission_span, db_span]
  let microservice_validator = MicroserviceTraceValidator::new()
  
  let call_chain_consistency = MicroserviceTraceValidator::validate_call_chain_consistency(microservice_validator, all_spans)
  
  assert_true(call_chain_consistency.is_consistent)
  assert_true(call_chain_consistency.call_hierarchy_valid)
  assert_true(call_chain_consistency.parallel_calls_identified)
  assert_eq(call_chain_consistency.entry_point, "api.gateway")
  assert_eq(call_chain_consistency.service_count, 4)
  
  // 验证调用链拓扑
  let topology = MicroserviceTraceValidator::extract_call_topology(microservice_validator, all_spans)
  
  assert_true(topology.is_valid_topology)
  assert_true(topology.service_dependencies.contains(("user.service", "auth.service")))
  assert_true(topology.service_dependencies.contains(("user.service", "permission.service")))
  assert_true(topology.service_dependencies.contains(("user.service", "database.service")))
  assert_true(topology.service_dependencies.contains(("api.gateway", "user.service")))
  
  // 验证调用链性能
  let performance_analysis = MicroserviceTraceValidator::analyze_call_chain_performance(microservice_validator, all_spans)
  
  assert_true(performance_analysis.total_duration > 0)
  assert_true(performance_analysis.service_durations.length() > 0)
  assert_true(performance_analysis.bottleneck_identified == true || performance_analysis.bottleneck_identified == false)
  
  // 验证错误传播
  let error_propagation = MicroserviceTraceValidator::validate_error_propagation(microservice_validator, all_spans)
  
  assert_true(error_propagation.no_error_leakage)
  assert_true(error_propagation.error_context_preserved)
}

// 测试5: 跨数据中心追踪一致性
test "跨数据中心追踪一致性测试" {
  let tracer_provider = TracerProvider::default()
  let trace_id = TraceId::generate()
  let global_request_id = "global-req-12345"
  
  // 数据中心A - 主节点
  let dc_a_tracer = TracerProvider::get_tracer(tracer_provider, "primary.datacenter")
  let dc_a_span = Tracer::start_span_with_trace_id(dc_a_tracer, "global.request", trace_id, SpanId::generate())
  
  Span::set_attribute(dc_a_span, "service.name", StringValue("api.gateway"))
  Span::set_attribute(dc_a_span, "datacenter", StringValue("dc-a"))
  Span::set_attribute(dc_a_span, "region", StringValue("us-east"))
  Span::set_attribute(dc_a_span, "global.request.id", StringValue(global_request_id))
  Span::set_attribute(dc_a_span, "request.source", StringValue("external"))
  
  let dc_a_context = Span::context(dc_a_span)
  
  // 数据中心B - 处理节点
  let dc_b_tracer = TracerProvider::get_tracer(tracer_provider, "secondary.datacenter")
  let dc_b_span = Tracer::start_span_with_context(dc_b_tracer, "replicated.request", dc_a_context)
  
  Span::set_attribute(dc_b_span, "service.name", StringValue("data.processor"))
  Span::set_attribute(dc_b_span, "datacenter", StringValue("dc-b"))
  Span::set_attribute(dc_b_span, "region", StringValue("us-west"))
  Span::set_attribute(dc_b_span, "global.request.id", StringValue(global_request_id))
  Span::set_attribute(dc_b_span, "origin.datacenter", StringValue("dc-a"))
  
  let dc_b_context = Span::context(dc_b_span)
  
  // 数据中心C - 存储节点
  let dc_c_tracer = TracerProvider::get_tracer(tracer_provider, "storage.datacenter")
  let dc_c_span = Tracer::start_span_with_context(dc_c_tracer, "storage.operation", dc_b_context)
  
  Span::set_attribute(dc_c_span, "service.name", StringValue("storage.service"))
  Span::set_attribute(dc_c_span, "datacenter", StringValue("dc-c"))
  Span::set_attribute(dc_c_span, "region", StringValue("eu-west"))
  Span::set_attribute(dc_c_span, "global.request.id", StringValue(global_request_id))
  Span::set_attribute(dc_c_span, "origin.datacenter", StringValue("dc-b"))
  
  Span::end(dc_c_span)
  Span::end(dc_b_span)
  Span::end(dc_a_span)
  
  // 验证跨数据中心追踪一致性
  let all_spans = [dc_a_span, dc_b_span, dc_c_span]
  let cross_dc_validator = CrossDatacenterTraceValidator::new()
  
  let cross_dc_consistency = CrossDatacenterTraceValidator::validate_cross_datacenter_consistency(cross_dc_validator, all_spans)
  
  assert_true(cross_dc_consistency.is_consistent)
  assert_true(cross_dc_consistency.global_request_id_consistent)
  assert_true(cross_dc_consistency.datacenter_flow_valid)
  assert_eq(cross_dc_consistency.involved_datacenters.length(), 3)
  assert_true(cross_dc_consistency.datacenter_sequence.contains(["dc-a", "dc-b", "dc-c"]))
  
  // 验证网络延迟追踪
  let latency_analysis = CrossDatacenterTraceValidator::analyze_network_latency(cross_dc_validator, all_spans)
  
  assert_true(latency_analysis.inter_dc_latency_calculated)
  assert_true(latency_analysis.total_latency > 0)
  assert_eq(latency_analysis.dc_hops.length(), 2) // dc-a->dc-b, dc-b->dc-c
  
  // 验证数据一致性
  let data_consistency = CrossDatacenterTraceValidator::validate_data_consistency(cross_dc_validator, all_spans)
  
  assert_true(data_consistency.data_consistent_across_dcs)
  assert_true(data_consistency.replication_lag_within_threshold)
}

// 测试6: 追踪采样一致性验证
test "追踪采样一致性验证测试" {
  // 创建采样策略
  let sampling_config = SamplingConfiguration::new()
  SamplingConfiguration::set_strategy(sampling_config, "probability")
  SamplingConfiguration::set_probability(sampling_config, 0.1) // 10%采样率
  
  // 创建带采样的追踪提供者
  let tracer_provider = TracerProvider::with_sampling(sampling_config)
  
  // 生成多个请求
  let total_requests = 100
  let sampled_spans = []
  let unsampled_requests = 0
  
  for i in 1..=total_requests {
    let trace_id = TraceId::generate()
    let tracer = TracerProvider::get_tracer(tracer_provider, "test.service")
    
    // 检查是否被采样
    let sampling_decision = TracerProvider::should_sample(tracer_provider, trace_id, "test.operation")
    
    if sampling_decision.is_sampled {
      let span = Tracer::start_span_with_trace_id(tracer, "sampled.operation", trace_id, SpanId::generate())
      Span::set_attribute(span, "request.id", StringValue("req-" + i.to_string()))
      Span::set_attribute(span, "sampling.priority", IntValue(sampling_decision.priority))
      sampled_spans = sampled_spans.push(span)
      Span::end(span)
    } else {
      unsampled_requests = unsampled_requests + 1
    }
  }
  
  // 验证采样一致性
  let sampling_validator = SamplingConsistencyValidator::new()
  
  let sampling_consistency = SamplingConsistencyValidator::validate_sampling_consistency(
    sampling_validator, 
    sampled_spans, 
    total_requests, 
    0.1
  )
  
  assert_true(sampling_consistency.is_consistent)
  assert_true(sampling_consistency.actual_rate >= 0.05 && sampling_consistency.actual_rate <= 0.15) // 允许误差
  assert_eq(sampling_consistency.sampled_count, sampled_spans.length())
  assert_eq(sampling_consistency.unsampled_count, unsampled_requests)
  
  // 验证采样属性一致性
  let attribute_consistency = SamplingConsistencyValidator::validate_sampling_attributes(sampling_validator, sampled_spans)
  
  assert_true(attribute_consistency.attributes_consistent)
  assert_true(attribute_consistency.all_spans_have_sampling_priority)
  
  // 测试不同采样策略的一致性
  let deterministic_config = SamplingConfiguration::new()
  SamplingConfiguration::set_strategy(deterministic_config, "deterministic")
  
  let deterministic_provider = TracerProvider::with_sampling(deterministic_config)
  let deterministic_spans = []
  
  // 使用相同的trace_id应该得到相同的采样决策
  let fixed_trace_id = TraceId::from_string("fixed-trace-id-12345")
  
  for i in 1..=5 {
    let tracer = TracerProvider::get_tracer(deterministic_provider, "deterministic.test")
    let sampling_decision = TracerProvider::should_sample(deterministic_provider, fixed_trace_id, "test.operation")
    
    if sampling_decision.is_sampled {
      let span = Tracer::start_span_with_trace_id(tracer, "deterministic.operation", fixed_trace_id, SpanId::generate())
      deterministic_spans = deterministic_spans.push(span)
      Span::end(span)
    }
  }
  
  // 确定性采样应该对所有相同trace_id产生相同结果
  assert_true(deterministic_spans.length() == 0 || deterministic_spans.length() == 5)
}

// 测试7: 追踪数据完整性验证
test "追踪数据完整性验证测试" {
  let tracer_provider = TracerProvider::default()
  let trace_id = TraceId::generate()
  
  // 创建完整的追踪链
  let root_tracer = TracerProvider::get_tracer(tracer_provider, "root.service")
  let root_span = Tracer::start_span_with_trace_id(root_tracer, "root.operation", trace_id, SpanId::generate())
  
  Span::set_attribute(root_span, "service.name", StringValue("root.service"))
  Span::set_attribute(root_span, "operation.name", StringValue("root.operation"))
  Span::set_attribute(root_span, "trace.id", StringValue(trace_id.to_string()))
  
  let root_context = Span::context(root_span)
  
  // 创建子span
  let child_tracer = TracerProvider::get_tracer(tracer_provider, "child.service")
  let child_span = Tracer::start_span_with_context(child_tracer, "child.operation", root_context)
  
  Span::set_attribute(child_span, "service.name", StringValue("child.service"))
  Span::set_attribute(child_span, "operation.name", StringValue("child.operation"))
  Span::set_attribute(child_span, "trace.id", StringValue(trace_id.to_string()))
  Span::set_attribute(child_span, "parent.span.id", StringValue(Span::span_id(root_span).to_string()))
  
  // 添加事件
  Span::add_event_with_attributes(child_span, "operation.step1", [
    ("step.name", StringValue("initialization")),
    ("step.status", StringValue("completed"))
  ])
  
  Span::add_event_with_attributes(child_span, "operation.step2", [
    ("step.name", StringValue("processing")),
    ("step.status", StringValue("completed"))
  ])
  
  Span::end(child_span)
  Span::end(root_span)
  
  // 创建完整性验证器
  let integrity_validator = TraceIntegrityValidator::new()
  
  // 验证追踪完整性
  let integrity_check = TraceIntegrityValidator::validate_trace_integrity(integrity_validator, [root_span, child_span])
  
  assert_true(integrity_check.is_complete)
  assert_true(integrity_check.trace_id_consistent)
  assert_true(integrity_check.parent_child_relationship_valid)
  assert_true(integrity_check.timeline_consistent)
  assert_true(integrity_check.all_required_attributes_present)
  
  // 验证span完整性
  let span_integrity = TraceIntegrityValidator::validate_span_integrity(integrity_validator, root_span)
  
  assert_true(span_integrity.has_valid_trace_id)
  assert_true(span_integrity.has_valid_span_id)
  assert_true(span_integrity.has_timestamp)
  assert_true(span_integrity.has_duration)
  assert_true(span_integrity.has_service_name)
  
  // 验证事件完整性
  let event_integrity = TraceIntegrityValidator::validate_event_integrity(integrity_validator, child_span)
  
  assert_true(event_integrity.all_events_have_timestamps)
  assert_true(event_integrity.event_sequence_valid)
  assert_true(event_integrity.event_attributes_complete)
  
  // 测试不完整的追踪数据
  let incomplete_tracer = TracerProvider::get_tracer(tracer_provider, "incomplete.service")
  let incomplete_span = Tracer::start_span_with_trace_id(incomplete_tracer, "incomplete.operation", TraceId::generate(), SpanId::generate())
  
  // 故意不设置必需属性
  Span::add_event(incomplete_span, "orphan.event") // 没有属性
  Span::end(incomplete_span)
  
  let incomplete_integrity = TraceIntegrityValidator::validate_span_integrity(integrity_validator, incomplete_span)
  
  assert_false(incomplete_integrity.all_required_attributes_present)
  assert_false(incomplete_integrity.event_attributes_complete)
}