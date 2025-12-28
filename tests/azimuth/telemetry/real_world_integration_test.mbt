// 实际使用场景集成测试
// 测试遥测系统在真实世界使用场景中的集成功能

test "Web应用请求处理集成测试" {
  // 模拟Web应用处理HTTP请求的完整流程
  
  // 1. 创建服务资源
  let service_resource = Resource::new()
  let service_attrs = [
    ("service.name", StringValue("web-api-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("web-api-instance-001")),
    ("deployment.environment", StringValue("production"))
  ]
  let web_service_resource = Resource::with_attributes(service_resource, service_attrs)
  
  // 2. 创建Tracer、Logger和Meter
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "web-api-tracer")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "web-api-logger")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "web-api-meter")
  
  // 3. 创建度量
  let request_counter = Meter::create_counter(meter, "http.requests.total",
    Some("Total HTTP requests"), Some("requests"))
  let response_histogram = Meter::create_histogram(meter, "http.response.duration",
    Some("HTTP response duration"), Some("ms"))
  let error_counter = Meter::create_counter(meter, "http.errors.total",
    Some("Total HTTP errors"), Some("errors"))
  
  // 4. 模拟接收HTTP请求
  let request_span_ctx = SpanContext::new("web-request-trace-12345", "web-request-span-67890", true, "")
  let request_span = Span::new("HTTP GET /api/users", Server, request_span_ctx)
  
  // 添加请求开始事件
  Span::add_event(request_span, "request.received", Some([
    ("http.method", StringValue("GET")),
    ("http.url", StringValue("/api/users")),
    ("http.scheme", StringValue("https")),
    ("http.host", StringValue("api.example.com")),
    ("user.agent", StringValue("Mozilla/5.0...")),
    ("remote.addr", StringValue("192.168.1.100"))
  ]))
  
  // 记录请求开始日志
  let request_log = LogRecord::new_with_context(
    Info,
    Some("Received HTTP request: GET /api/users"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(Clock::now_unix_nanos(Clock::system()) + 1L),
    Some("web-request-trace-12345"),
    Some("web-request-span-67890"),
    Some(Context::root())
  )
  Logger::emit(logger, request_log)
  
  // 5. 模拟数据库查询
  let db_span_ctx = SpanContext::new("web-request-trace-12345", "db-query-span-11111", true, "")
  let db_span = Span::new("database.query", Client, db_span_ctx)
  
  Span::add_event(db_span, "query.started", Some([
    ("db.system", StringValue("postgresql")),
    ("db.statement", StringValue("SELECT * FROM users WHERE active = true")),
    ("db.operation", StringValue("SELECT"))
  ]))
  
  // 模拟查询延迟
  Span::add_event(db_span, "query.completed", Some([
    ("db.rows_affected", IntValue(25)),
    ("db.duration_ms", IntValue(45))
  ]))
  
  Span::end(db_span)
  
  // 6. 模拟缓存查询
  let cache_span_ctx = SpanContext::new("web-request-trace-12345", "cache-query-span-22222", true, "")
  let cache_span = Span::new("cache.get", Internal, cache_span_ctx)
  
  Span::add_event(cache_span, "cache.hit", Some([
    ("cache.system", StringValue("redis")),
    ("cache.key", StringValue("user:profile:12345")),
    ("cache.hit", BoolValue(true))
  ]))
  
  Span::end(cache_span)
  
  // 7. 模拟业务逻辑处理
  let business_span_ctx = SpanContext::new("web-request-trace-12345", "business-span-33333", true, "")
  let business_span = Span::new("business.process_user_data", Internal, business_span_ctx)
  
  Span::add_event(business_span, "validation.started", Some([
    ("validation.step", StringValue("input-sanitization")),
    ("validation.rules", IntValue(5))
  ]))
  
  Span::add_event(business_span, "validation.completed", Some([
    ("validation.result", StringValue("passed")),
    ("validation.errors", IntValue(0))
  ]))
  
  Span::add_event(business_span, "transformation.started", Some([
    ("transformation.type", StringValue("user-profile-enrichment"))
  ]))
  
  Span::add_event(business_span, "transformation.completed", Some([
    ("transformation.fields_added", IntValue(3)),
    ("transformation.duration_ms", IntValue(12))
  ]))
  
  Span::end(business_span)
  
  // 8. 模拟响应生成
  Span::add_event(request_span, "response.generated", Some([
    ("http.status_code", IntValue(200)),
    ("http.response.size", IntValue(2048)),
    ("content.type", StringValue("application/json"))
  ]))
  
  // 9. 记录度量
  let request_attrs = Attributes::new()
  Attributes::set(request_attrs, "http.method", StringValue("GET"))
  Attributes::set(request_attrs, "http.route", StringValue("/api/users"))
  Attributes::set(request_attrs, "http.status_code", IntValue(200))
  
  Counter::add(request_counter, 1.0, Some(request_attrs))
  Histogram::record(response_histogram, 125.5, Some(request_attrs))
  
  // 10. 记录成功响应日志
  let response_log = LogRecord::new_with_context(
    Info,
    Some("HTTP request completed successfully: 200 GET /api/users (125.5ms)"),
    Some(request_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(Clock::now_unix_nanos(Clock::system()) + 1L),
    Some("web-request-trace-12345"),
    Some("web-request-span-67890"),
    Some(Context::root())
  )
  Logger::emit(logger, response_log)
  
  // 11. 结束请求Span
  Span::set_status(request_span, Ok, Some("Request processed successfully"))
  Span::end(request_span)
}

test "微服务调用链集成测试" {
  // 模拟微服务架构中的调用链
  
  // 1. 服务A：API Gateway
  let gateway_tracer = TracerProvider::get_tracer(TracerProvider::default(), "api-gateway")
  let gateway_span_ctx = SpanContext::new("microservice-trace-abc", "gateway-span-123", true, "")
  let gateway_span = Span::new("API Gateway: Process order request", Server, gateway_span_ctx)
  
  Span::add_event(gateway_span, "authentication.started", Some([
    ("auth.method", StringValue("JWT")),
    ("auth.user_id", StringValue("user-12345"))
  ]))
  
  Span::add_event(gateway_span, "authentication.completed", Some([
    ("auth.result", StringValue("success")),
    ("auth.duration_ms", IntValue(15)
  ]))
  
  // 2. 服务A调用服务B：Order Service
  let order_carrier = TextMapCarrier::new()
  let composite_propagator = CompositePropagator::new([
    W3CTraceContextPropagator::new(),
    W3CBaggagePropagator::new()
  ])
  
  let gateway_ctx = Context::root()
  CompositePropagator::inject(composite_propagator, gateway_ctx, order_carrier)
  TextMapCarrier::set(order_carrier, "X-Request-ID", "req-order-001")
  TextMapCarrier::set(order_carrier, "baggage", "user.id=user-12345,request.type=order")
  
  // 模拟HTTP调用到Order Service
  let order_request = HttpRequest::new(
    "POST",
    "https://order-service.example.com/api/orders",
    [("Content-Type", "application/json"), ("X-Request-ID", "req-order-001")],
    Some("{\"product_id\":\"prod-123\",\"quantity\":2}")
  )
  
  // 3. 服务B：Order Service处理
  let order_tracer = TracerProvider::get_tracer(TracerProvider::default(), "order-service")
  let order_span_ctx = SpanContext::new("microservice-trace-abc", "order-span-456", true, "")
  let order_span = Span::new("Order Service: Create order", Server, order_span_ctx)
  
  Span::add_event(order_span, "validation.started", Some([
    ("validation.type", StringValue("order-data")),
    ("product_id", StringValue("prod-123")),
    ("quantity", IntValue(2))
  ]))
  
  // 4. 服务B调用服务C：Inventory Service
  let inventory_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, Context::root(), inventory_carrier)
  TextMapCarrier::set(inventory_carrier, "X-Request-ID", "req-order-001")
  TextMapCarrier::set(inventory_carrier, "baggage", "user.id=user-12345,request.type=order,service=order")
  
  let inventory_request = HttpRequest::new(
    "GET",
    "https://inventory-service.example.com/api/inventory/prod-123",
    [("X-Request-ID", "req-order-001")],
    None
  )
  
  // 5. 服务C：Inventory Service处理
  let inventory_tracer = TracerProvider::get_tracer(TracerProvider::default(), "inventory-service")
  let inventory_span_ctx = SpanContext::new("microservice-trace-abc", "inventory-span-789", true, "")
  let inventory_span = Span::new("Inventory Service: Check stock", Server, inventory_span_ctx)
  
  Span::add_event(inventory_span, "database.query", Some([
    ("db.operation", StringValue("SELECT")),
    ("db.table", StringValue("inventory")),
    ("product_id", StringValue("prod-123"))
  ]))
  
  Span::add_event(inventory_span, "stock.checked", Some([
    ("stock.available", IntValue(10)),
    ("stock.reserved", IntValue(3)),
    ("stock.result", StringValue("available"))
  ]))
  
  Span::set_status(inventory_span, Ok, Some("Stock check completed"))
  Span::end(inventory_span)
  
  // 6. Inventory Service响应
  let inventory_response = HttpResponse::new(
    200,
    [("Content-Type", "application/json"), ("X-Response-ID", "resp-inventory-001")],
    Some("{\"product_id\":\"prod-123\",\"available\":10,\"reserved\":3}")
  )
  
  // 7. Order Service继续处理
  Span::add_event(order_span, "inventory.checked", Some([
    ("inventory.result", StringValue("success")),
    ("inventory.available", IntValue(10))
  ]))
  
  Span::add_event(order_span, "order.created", Some([
    ("order_id", StringValue("order-45678")),
    ("product_id", StringValue("prod-123")),
    ("quantity", IntValue(2)),
    ("total_amount", FloatValue(199.98))
  ]))
  
  Span::set_status(order_span, Ok, Some("Order created successfully"))
  Span::end(order_span)
  
  // 8. Order Service响应
  let order_response = HttpResponse::new(
    201,
    [("Content-Type", "application/json"), ("X-Response-ID", "resp-order-001")],
    Some("{\"order_id\":\"order-45678\",\"status\":\"created\"}")
  )
  
  // 9. API Gateway完成处理
  Span::add_event(gateway_span, "order.completed", Some([
    ("order_id", StringValue("order-45678")),
    ("order_status", StringValue("created"))
  ]))
  
  Span::set_status(gateway_span, Ok, Some("Order request processed successfully"))
  Span::end(gateway_span)
}

test "消息队列处理集成测试" {
  // 模拟消息队列处理流程
  
  // 1. 消息生产者
  let producer_tracer = TracerProvider::get_tracer(TracerProvider::default(), "message-producer")
  let producer_logger = LoggerProvider::get_logger(LoggerProvider::default(), "message-producer")
  let producer_meter = MeterProvider::get_meter(MeterProvider::default(), "message-producer")
  
  let producer_span_ctx = SpanContext::new("message-trace-xyz", "producer-span-111", true, "")
  let producer_span = Span::new("Message Producer: Publish user event", Producer, producer_span_ctx)
  
  // 2. 创建消息
  let message_payload = "{\"user_id\":\"user-123\",\"event_type\":\"profile_updated\",\"timestamp\":1735689600}"
  
  Span::add_event(producer_span, "message.created", Some([
    ("message.type", StringValue("user-event")),
    ("message.event_type", StringValue("profile_updated")),
    ("message.user_id", StringValue("user-123")),
    ("message.size", IntValue(message_payload.length()))
  ]))
  
  // 3. 发布消息到队列
  Span::add_event(producer_span, "message.published", Some([
    ("messaging.system", StringValue("kafka")),
    ("messaging.destination", StringValue("user-events-topic")),
    ("messaging.message_id", StringValue("msg-456789")),
    ("messaging.partition", IntValue(2))
  ]))
  
  // 4. 记录生产者度量
  let message_counter = Meter::create_counter(producer_meter, "messages.published",
    Some("Messages published"), Some("messages"))
  
  let producer_attrs = Attributes::new()
  Attributes::set(producer_attrs, "messaging.system", StringValue("kafka"))
  Attributes::set(producer_attrs, "messaging.destination", StringValue("user-events-topic"))
  Attributes::set(producer_attrs, "message.event_type", StringValue("profile_updated"))
  
  Counter::add(message_counter, 1.0, Some(producer_attrs))
  
  // 5. 记录生产者日志
  let producer_log = LogRecord::new_with_context(
    Info,
    Some("Published user event message to Kafka topic: user-events-topic"),
    Some(producer_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(Clock::now_unix_nanos(Clock::system()) + 1L),
    Some("message-trace-xyz"),
    Some("producer-span-111"),
    Some(Context::root())
  )
  Logger::emit(producer_logger, producer_log)
  
  Span::end(producer_span)
  
  // 6. 消息消费者
  let consumer_tracer = TracerProvider::get_tracer(TracerProvider::default(), "message-consumer")
  let consumer_logger = LoggerProvider::get_logger(LoggerProvider::default(), "message-consumer")
  let consumer_meter = MeterProvider::get_meter(MeterProvider::default(), "message-consumer")
  
  let consumer_span_ctx = SpanContext::new("message-trace-xyz", "consumer-span-222", true, "")
  let consumer_span = Span::new("Message Consumer: Process user event", Consumer, consumer_span_ctx)
  
  // 7. 接收消息
  Span::add_event(consumer_span, "message.received", Some([
    ("messaging.system", StringValue("kafka")),
    ("messaging.destination", StringValue("user-events-topic")),
    ("messaging.message_id", StringValue("msg-456789")),
    ("messaging.partition", IntValue(2)),
    ("messaging.offset", IntValue(12345))
  ]))
  
  // 8. 处理消息
  Span::add_event(consumer_span, "processing.started", Some([
    ("processing.operation", StringValue("user-profile-update")),
    ("user.id", StringValue("user-123"))
  ]))
  
  // 9. 调用数据库更新
  let db_span_ctx = SpanContext::new("message-trace-xyz", "db-update-span-333", true, "")
  let db_span = Span::new("database.update", Client, db_span_ctx)
  
  Span::add_event(db_span, "update.started", Some([
    ("db.system", StringValue("postgresql")),
    ("db.table", StringValue("user_profiles")),
    ("db.operation", StringValue("UPDATE")),
    ("user.id", StringValue("user-123"))
  ]))
  
  Span::add_event(db_span, "update.completed", Some([
    ("db.rows_affected", IntValue(1)),
    ("db.duration_ms", IntValue(25))
  ]))
  
  Span::end(db_span)
  
  // 10. 发送通知
  let notification_span_ctx = SpanContext::new("message-trace-xyz", "notification-span-444", true, "")
  let notification_span = Span::new("notification.send", Client, notification_span_ctx)
  
  Span::add_event(notification_span, "notification.sent", Some([
    ("notification.channel", StringValue("email")),
    ("notification.recipient", StringValue("user@example.com")),
    ("notification.template", StringValue("profile-updated"))
  ]))
  
  Span::end(notification_span)
  
  // 11. 完成消息处理
  Span::add_event(consumer_span, "processing.completed", Some([
    ("processing.result", StringValue("success")),
    ("processing.duration_ms", IntValue(150))
  ]))
  
  // 12. 记录消费者度量
  let processed_counter = Meter::create_counter(consumer_meter, "messages.processed",
    Some("Messages processed"), Some("messages"))
  
  let consumer_attrs = Attributes::new()
  Attributes::set(consumer_attrs, "messaging.system", StringValue("kafka"))
  Attributes::set(consumer_attrs, "messaging.destination", StringValue("user-events-topic"))
  Attributes::set(consumer_attrs, "processing.result", StringValue("success"))
  
  Counter::add(processed_counter, 1.0, Some(consumer_attrs))
  
  // 13. 记录消费者日志
  let consumer_log = LogRecord::new_with_context(
    Info,
    Some("Successfully processed user event message for user: user-123"),
    Some(consumer_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(Clock::now_unix_nanos(Clock::system()) + 1L),
    Some("message-trace-xyz"),
    Some("consumer-span-222"),
    Some(Context::root())
  )
  Logger::emit(consumer_logger, consumer_log)
  
  Span::set_status(consumer_span, Ok, Some("Message processed successfully"))
  Span::end(consumer_span)
}

test "批处理作业集成测试" {
  // 模拟批处理作业的完整流程
  
  // 1. 创建批处理资源
  let batch_resource = Resource::new()
  let batch_attrs = [
    ("service.name", StringValue("batch-processor")),
    ("service.version", StringValue("1.5.2")),
    ("job.name", StringValue("daily-report-generation")),
    ("job.id", StringValue("batch-job-20251228-001"))
  ]
  let batch_service_resource = Resource::with_attributes(batch_resource, batch_attrs)
  
  // 2. 创建Tracer、Logger和Meter
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "batch-tracer")
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "batch-logger")
  let meter = MeterProvider::get_meter(MeterProvider::default(), "batch-meter")
  
  // 3. 创建度量
  let records_processed_counter = Meter::create_counter(meter, "batch.records.processed",
    Some("Records processed by batch job"), Some("records"))
  let processing_time_histogram = Meter::create_histogram(meter, "batch.processing.time",
    Some("Batch job processing time"), Some("ms"))
  let error_counter = Meter::create_counter(meter, "batch.errors",
    Some("Batch job errors"), Some("errors"))
  
  // 4. 开始批处理作业
  let batch_span_ctx = SpanContext::new("batch-trace-20251228", "batch-span-main", true, "")
  let batch_span = Span::new("Batch Job: Daily Report Generation", Internal, batch_span_ctx)
  
  Span::add_event(batch_span, "job.started", Some([
    ("job.type", StringValue("daily-report")),
    ("job.scheduled_time", StringValue("2025-12-28T02:00:00Z")),
    ("job.start_time", StringValue("2025-12-28T02:00:05Z"))
  ]))
  
  // 5. 数据提取阶段
  let extract_span_ctx = SpanContext::new("batch-trace-20251228", "batch-span-extract", true, "")
  let extract_span = Span::new("Data Extraction", Internal, extract_span_ctx)
  
  Span::add_event(extract_span, "extraction.started", Some([
    ("extraction.source", StringValue("database")),
    ("extraction.table", StringValue("orders")),
    ("extraction.date_range", StringValue("2025-12-27"))
  ]))
  
  // 模拟提取记录
  let total_records = 50000
  for batch_num in range(0, 10) {
    Span::add_event(extract_span, "batch.extracted", Some([
      ("batch.number", IntValue(batch_num)),
      ("batch.size", IntValue(5000)),
      ("records.extracted", IntValue((batch_num + 1) * 5000))
    ]))
  }
  
  Span::add_event(extract_span, "extraction.completed", Some([
    ("total.records", IntValue(total_records)),
    ("extraction.duration_ms", IntValue(1200))
  ]))
  
  Span::end(extract_span)
  
  // 6. 数据转换阶段
  let transform_span_ctx = SpanContext::new("batch-trace-20251228", "batch-span-transform", true, "")
  let transform_span = Span::new("Data Transformation", Internal, transform_span_ctx)
  
  Span::add_event(transform_span, "transformation.started", Some([
    ("transformation.type", StringValue("aggregation")),
    ("input.records", IntValue(total_records))
  ]))
  
  // 模拟转换步骤
  let transformation_steps = ["filtering", "aggregation", "formatting", "validation"]
  for step in transformation_steps {
    Span::add_event(transform_span, "step.completed", Some([
      ("step.name", StringValue(step)),
      ("step.duration_ms", IntValue(300))
    ]))
  }
  
  Span::add_event(transform_span, "transformation.completed", Some([
    ("output.records", IntValue(100)),
    ("transformation.duration_ms", IntValue(1200))
  ]))
  
  Span::end(transform_span)
  
  // 7. 报告生成阶段
  let report_span_ctx = SpanContext::new("batch-trace-20251228", "batch-span-report", true, "")
  let report_span = Span::new("Report Generation", Internal, report_span_ctx)
  
  Span::add_event(report_span, "generation.started", Some([
    ("report.type", StringValue("daily-sales")),
    ("report.format", StringValue("PDF")),
    ("template", StringValue("sales-daily-template-v2"))
  ]))
  
  // 模拟报告生成
  let report_sections = ["summary", "details", "charts", "appendix"]
  for section in report_sections {
    Span::add_event(report_span, "section.generated", Some([
      ("section.name", StringValue(section)),
      ("section.size_kb", IntValue(1024))
    ]))
  }
  
  Span::add_event(report_span, "generation.completed", Some([
    ("report.size_mb", FloatValue(5.2)),
    ("report.pages", IntValue(25)),
    ("generation.duration_ms", IntValue(800))
  ]))
  
  Span::end(report_span)
  
  // 8. 分发阶段
  let distribute_span_ctx = SpanContext::new("batch-trace-20251228", "batch-span-distribute", true, "")
  let distribute_span = Span::new("Report Distribution", Internal, distribute_span_ctx)
  
  Span::add_event(distribute_span, "distribution.started", Some([
    ("distribution.method", StringValue("email")),
    ("recipients", IntValue(15))
  ]))
  
  // 模拟发送报告
  for i in range(0, 15) {
    Span::add_event(distribute_span, "email.sent", Some([
      ("recipient.id", IntValue(i + 1)),
      ("email.address", StringValue("manager" + (i + 1).to_string() + "@company.com"))
    ]))
  }
  
  Span::add_event(distribute_span, "distribution.completed", Some([
    ("emails.sent", IntValue(15)),
    ("emails.failed", IntValue(0)),
    ("distribution.duration_ms", IntValue(300))
  ]))
  
  Span::end(distribute_span)
  
  // 9. 记录批处理度量
  let batch_attrs = Attributes::new()
  Attributes::set(batch_attrs, "job.name", StringValue("daily-report-generation"))
  Attributes::set(batch_attrs, "job.status", StringValue("completed"))
  
  Counter::add(records_processed_counter, total_records.to_double(), Some(batch_attrs))
  Histogram::record(processing_time_histogram, 3500.0, Some(batch_attrs))
  
  // 10. 记录批处理日志
  let completion_log = LogRecord::new_with_context(
    Info,
    Some("Batch job completed successfully: processed 50000 records, generated 25-page report"),
    Some(batch_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(Clock::now_unix_nanos(Clock::system()) + 1L),
    Some("batch-trace-20251228"),
    Some("batch-span-main"),
    Some(Context::root())
  )
  Logger::emit(logger, completion_log)
  
  // 11. 完成批处理作业
  Span::add_event(batch_span, "job.completed", Some([
    ("job.status", StringValue("success")),
    ("total.duration_ms", IntValue(3500)),
    ("records.processed", IntValue(total_records)),
    ("reports.generated", IntValue(1)),
    ("notifications.sent", IntValue(15))
  ]))
  
  Span::set_status(batch_span, Ok, Some("Batch job completed successfully"))
  Span::end(batch_span)
}