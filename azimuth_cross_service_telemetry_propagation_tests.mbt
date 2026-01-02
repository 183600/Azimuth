// Azimuth 跨服务遥测传播测试用例
// 专注于遥测数据在微服务架构中的传播、转换和关联

// 测试1: HTTP服务间遥测传播测试
test "HTTP服务间遥测传播测试" {
  let service_a = HTTPService::new("service-a", 8080)
  let service_b = HTTPService::new("service-b", 8081)
  let service_c = HTTPService::new("service-c", 8082)
  
  // 配置遥测传播
  HTTPService::enable_telemetry(service_a)
  HTTPService::enable_telemetry(service_b)
  HTTPService::enable_telemetry(service_c)
  
  // 模拟服务A接收外部请求
  let incoming_headers = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("x-request-id", "req-12345"),
    ("x-user-id", "user-67890")
  ]
  
  let request_context = HTTPService::create_request_context(service_a, incoming_headers)
  let span_a = HTTPService::start_span(service_a, "api.request", request_context)
  
  // 服务A调用服务B
  let headers_to_b = HTTPService::extract_telemetry_headers(service_a, span_a)
  Array::push(headers_to_b, ("x-service-a-timestamp", Time::now().to_string()))
  
  let response_b = HTTPService::call_service(service_b, "/api/endpoint", headers_to_b)
  let span_b = HTTPService::get_span_from_response(service_b, response_b)
  
  // 服务B调用服务C
  let headers_to_c = HTTPService::extract_telemetry_headers(service_b, span_b)
  Array::push(headers_to_c, ("x-service-b-processing-time", "150ms"))
  
  let response_c = HTTPService::call_service(service_c, "/api/process", headers_to_c)
  let span_c = HTTPService::get_span_from_response(service_c, response_c)
  
  // 完成所有span
  Span::end(span_c)
  Span::end(span_b)
  Span::end(span_a)
  
  // 验证追踪上下文传播
  let trace_id_a = Span::trace_id(span_a)
  let trace_id_b = Span::trace_id(span_b)
  let trace_id_c = Span::trace_id(span_c)
  
  assert_eq(trace_id_a, trace_id_b)
  assert_eq(trace_id_b, trace_id_c)
  
  // 验证父子关系
  let parent_id_b = Span::parent_span_id(span_b)
  let parent_id_c = Span::parent_span_id(span_c)
  
  assert_eq(parent_id_b, Span::span_id(span_a))
  assert_eq(parent_id_c, Span::span_id(span_b))
  
  // 验证 baggage 传播
  let baggage_a = Span::get_baggage(span_a)
  let baggage_b = Span::get_baggage(span_b)
  let baggage_c = Span::get_baggage(span_c)
  
  assert_eq(Baggage::get(baggage_a, "user-id"), Some("user-67890"))
  assert_eq(Baggage::get(baggage_b, "user-id"), Some("user-67890"))
  assert_eq(Baggage::get(baggage_c, "user-id"), Some("user-67890"))
}

// 测试2: 异步消息队列遥测传播测试
test "异步消息队列遥测传播测试" {
  let message_producer = MessageProducer::new("order-service")
  let message_consumer = MessageConsumer::new("inventory-service")
  let telemetry_propagator = MessageTelemetryPropagator::new()
  
  // 配置消息遥测传播
  MessageProducer::enable_telemetry(message_producer, telemetry_propagator)
  MessageConsumer::enable_telemetry(message_consumer, telemetry_propagator)
  
  // 生产者创建span
  let producer_span = MessageProducer::start_span(message_producer, "order.processing")
  Span::set_attribute(producer_span, "order.id", StringValue("order-12345"))
  Span::set_attribute(producer_span, "customer.id", StringValue("customer-67890"))
  
  // 注入遥测上下文到消息
  let order_message = Message::new("order.created", {
    "order_id" => "order-12345",
    "items" => "[{\"product_id\": \"prod-1\", \"quantity\": 2}]"
  })
  
  let enriched_message = MessageTelemetryPropagator::inject(
    telemetry_propagator, 
    order_message, 
    producer_span
  )
  
  // 发送消息
  MessageProducer::send(message_producer, "inventory.queue", enriched_message)
  Span::end(producer_span)
  
  // 消费者接收并提取遥测上下文
  let received_message = MessageConsumer::receive(message_consumer, "inventory.queue")
  let extracted_context = MessageTelemetryPropagator::extract(
    telemetry_propagator, 
    received_message
  )
  
  // 消费者创建子span
  let consumer_span = MessageConsumer::start_span_with_context(
    message_consumer, 
    "inventory.update", 
    extracted_context
  )
  
  Span::set_attribute(consumer_span, "processing.time.ms", IntValue(250))
  Span::add_event(consumer_span, "stock.checked", None)
  Span::set_status(consumer_span, Ok)
  Span::end(consumer_span)
  
  // 验证消息遥测传播
  let producer_trace_id = Span::trace_id(producer_span)
  let consumer_trace_id = Span::trace_id(consumer_span)
  
  assert_eq(producer_trace_id, consumer_trace_id)
  
  // 验证消息关联
  let consumer_parent_id = Span::parent_span_id(consumer_span)
  assert_eq(consumer_parent_id, Span::span_id(producer_span))
  
  // 验证消息属性传播
  let order_id_attr = Span::get_attribute(consumer_span, "messaging.message_id")
  assert_true(order_id_attr.is_some())
  
  // 验证消息时间戳
  let message_timestamp = Message::get_timestamp(received_message)
  let consumer_start_time = Span::start_time(consumer_span)
  assert_true(Time::is_after_or_equal(consumer_start_time, message_timestamp))
}

// 测试3: 数据库操作遥测传播测试
test "数据库操作遥测传播测试" {
  let application_service = Service::new("api-service")
  let database_service = DatabaseService::new("postgres-cluster")
  let db_telemetry_propagator = DatabaseTelemetryPropagator::new()
  
  // 配置数据库遥测
  Service::enable_database_telemetry(application_service, db_telemetry_propagator)
  DatabaseService::enable_telemetry(database_service)
  
  // 应用服务创建业务操作span
  let business_span = Service::start_span(application_service, "user.profile.update")
  Span::set_attribute(business_span, "user.id", StringValue("user-12345"))
  
  // 应用服务调用数据库
  let db_context = DatabaseTelemetryPropagator::create_db_context(
    db_telemetry_propagator, 
    business_span
  )
  
  let db_query = "UPDATE users SET name = $1, email = $2 WHERE id = $3"
  let db_params = ["John Doe", "john.doe@example.com", "user-12345"]
  
  // 数据库服务创建数据库操作span
  let db_span = DatabaseService::execute_query(
    database_service, 
    db_query, 
    db_params, 
    db_context
  )
  
  Span::set_attribute(db_span, "db.statement", StringValue(db_query))
  Span::set_attribute(db_span, "db.type", StringValue("postgresql"))
  Span::set_attribute(db_span, "db.connection_pool", StringValue("primary"))
  
  // 模拟数据库执行时间
  Thread::sleep(Duration::from_ms(50))
  Span::set_status(db_span, Ok)
  Span::end(db_span)
  
  // 应用服务继续业务逻辑
  Span::add_event(business_span, "cache.invalidated", None)
  Span::set_status(business_span, Ok)
  Span::end(business_span)
  
  // 验证数据库遥测传播
  let business_trace_id = Span::trace_id(business_span)
  let db_trace_id = Span::trace_id(db_span)
  
  assert_eq(business_trace_id, db_trace_id)
  
  // 验证数据库span属性
  let db_statement = Span::get_attribute(db_span, "db.statement")
  let db_type = Span::get_attribute(db_span, "db.type")
  let db_rows_affected = Span::get_attribute(db_span, "db.rows_affected")
  
  assert_eq(db_statement, Some(StringValue(db_query)))
  assert_eq(db_type, Some(StringValue("postgresql")))
  assert_eq(db_rows_affected, Some(IntValue(1)))
  
  // 验证父子关系
  let db_parent_id = Span::parent_span_id(db_span)
  assert_eq(db_parent_id, Span::span_id(business_span))
  
  // 验证数据库连接池追踪
  let pool_stats = DatabaseService::get_connection_pool_stats(database_service)
  assert_true(PoolStats::get_traced_connections(pool_stats) > 0)
  assert_eq(PoolStats::get_last_trace_id(pool_stats), business_trace_id)
}

// 测试4: 跨服务遥测上下文转换测试
test "跨服务遥测上下文转换测试" {
  let context_converter = TelemetryContextConverter::new()
  let legacy_system = LegacySystem::new("legacy.service")
  let modern_system = ModernSystem::new("modern.service")
  
  // 现代系统创建标准遥测上下文
  let modern_tracer = ModernSystem::get_tracer(modern_system)
  let modern_span = ModernTracer::start_span(modern_tracer, "modern.operation")
  
  Span::set_attribute(modern_span, "service.version", StringValue("2.0.0"))
  Span::set_attribute(modern_span, "deployment.environment", StringValue("production"))
  
  // 转换为遗留系统兼容的上下文
  let legacy_context = TelemetryContextConverter::to_legacy(
    context_converter, 
    modern_span
  )
  
  // 遗留系统使用转换后的上下文
  let legacy_span = LegacySystem::start_span_with_context(
    legacy_system, 
    "legacy.operation", 
    legacy_context
  )
  
  LegacySpan::set_attribute(legacy_span, "legacy.format", StringValue("v1"))
  LegacySpan::set_attribute(legacy_span, "correlation.id", StringValue("corr-12345"))
  
  // 遗留系统调用另一个现代系统
  let back_to_modern_context = TelemetryContextConverter::from_legacy(
    context_converter, 
    legacy_span
  )
  
  let modern_span2 = ModernTracer::start_span_with_context(
    modern_tracer, 
    "modern.operation.2", 
    back_to_modern_context
  )
  
  Span::set_attribute(modern_span2, "callback.operation", StringValue(true))
  
  // 完成所有span
  Span::end(modern_span2)
  LegacySpan::end(legacy_span)
  Span::end(modern_span)
  
  // 验证上下文转换的完整性
  let original_trace_id = Span::trace_id(modern_span)
  let legacy_trace_id = LegacySpan::trace_id(legacy_span)
  let converted_trace_id = Span::trace_id(modern_span2)
  
  assert_eq(original_trace_id, legacy_trace_id)
  assert_eq(legacy_trace_id, converted_trace_id)
  
  // 验证属性转换
  let legacy_version = LegacySpan::get_attribute(legacy_span, "service.version")
  assert_eq(legacy_version, Some(StringValue("2.0.0")))
  
  let converted_correlation = Span::get_attribute(modern_span2, "correlation.id")
  assert_eq(converted_correlation, Some(StringValue("corr-12345")))
  
  // 验证转换统计
  let conversion_stats = TelemetryContextConverter::get_stats(context_converter)
  assert_eq(ConversionStats::modern_to_legacy_count(conversion_stats), 1)
  assert_eq(ConversionStats::legacy_to_modern_count(conversion_stats), 1)
  assert_eq(ConversionStats::total_conversions(conversion_stats), 2)
}