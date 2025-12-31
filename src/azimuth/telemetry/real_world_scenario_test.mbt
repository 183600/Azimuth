// 实际使用场景测试 - 模拟真实世界的遥测使用场景

test "web_request_telemetry_scenario" {
  // 模拟Web请求处理的完整遥测场景
  
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let logger_provider = NoopLoggerProvider::{}
  let meter_provider = NoopMeterProvider::{}
  
  let tracer = tracer_provider.get_tracer("web-server", Some("1.0.0"))
  let logger = logger_provider.get_logger("web-server", Some("1.0.0"))
  let meter = meter_provider.get_meter("web-server", Some("1.0.0"))
  
  // 创建指标仪器
  let request_counter = meter.create_counter("http_requests_total", Some("requests"), Some("Total HTTP requests"))
  let request_duration = meter.create_histogram("http_request_duration_seconds", Some("seconds"), Some("HTTP request duration"))
  let active_connections = meter.create_up_down_counter("http_active_connections", Some("connections"), Some("Active HTTP connections"))
  let response_size = meter.create_histogram("http_response_size_bytes", Some("bytes"), Some("HTTP response size"))
  
  // 模拟请求开始
  active_connections.add(1L, Some([("server.endpoint", AttributeValue::string("/api/users"))]))
  
  let (request_ctx, request_span) = tracer.start_span(ctx, "HTTP GET /api/users", Server, Some([
    ("http.method", AttributeValue::string("GET")),
    ("http.target", AttributeValue::string("/api/users")),
    ("http.scheme", AttributeValue::string("https")),
    ("http.host", AttributeValue::string("api.example.com")),
    ("user.agent", AttributeValue::string("Mozilla/5.0...")),
    ("net.peer.ip", AttributeValue::string("192.168.1.100"))
  ]))
  
  // 记录请求开始
  request_counter.add(1L, Some([
    ("http.method", AttributeValue::string("GET")),
    ("http.target", AttributeValue::string("/api/users")),
    ("http.status_code", AttributeValue::int(200L))
  ]))
  
  logger.info("Request started", Some([
    ("request.id", AttributeValue::string("req-12345")),
    ("http.method", AttributeValue::string("GET")),
    ("http.target", AttributeValue::string("/api/users")),
    ("user.id", AttributeValue::string("user-67890"))
  ]))
  
  // 模拟数据库查询
  let (db_ctx, db_span) = tracer.start_span(request_ctx, "database.query", Internal, Some([
    ("db.system", AttributeValue::string("postgresql")),
    ("db.statement", AttributeValue::string("SELECT * FROM users WHERE id = $1")),
    ("db.operation", AttributeValue::string("SELECT"))
  ]))
  
  logger.debug("Database query started", Some([
    ("db.query.id", AttributeValue::string("query-abc123")),
    ("db.statement", AttributeValue::string("SELECT * FROM users WHERE id = $1"))
  ]))
  
  // 模拟数据库查询完成
  let db_duration_histogram = meter.create_histogram("db_query_duration_seconds", Some("seconds"), Some("Database query duration"))
  db_duration_histogram.record(0.025, Some([
    ("db.system", AttributeValue::string("postgresql")),
    ("db.operation", AttributeValue::string("SELECT"))
  ]))
  
  logger.debug("Database query completed", Some([
    ("db.query.id", AttributeValue::string("query-abc123")),
    ("db.rows_affected", AttributeValue::int(1L)),
    ("db.duration_ms", AttributeValue::int(25L))
  ]))
  
  // 模拟缓存操作
  let (cache_ctx, cache_span) = tracer.start_span(request_ctx, "cache.get", Internal, Some([
    ("cache.system", AttributeValue::string("redis")),
    ("cache.operation", AttributeValue::string("GET")),
    ("cache.key", AttributeValue::string("user:67890:profile"))
  ]))
  
  let cache_counter = meter.create_counter("cache_operations_total", Some("operations"), Some("Total cache operations"))
  cache_counter.add(1L, Some([
    ("cache.system", AttributeValue::string("redis")),
    ("cache.operation", AttributeValue::string("GET")),
    ("cache.hit", AttributeValue::bool(true))
  ]))
  
  logger.debug("Cache hit", Some([
    ("cache.key", AttributeValue::string("user:67890:profile")),
    ("cache.hit", AttributeValue::bool(true))
  ]))
  
  // 模拟响应生成
  let response_body_size = 1024L
  response_size.record(response_body_size.to_double(), Some([
    ("http.method", AttributeValue::string("GET")),
    ("http.target", AttributeValue::string("/api/users")),
    ("http.status_code", AttributeValue::int(200L))
  ]))
  
  // 记录请求完成
  let total_duration = 0.15  // 150ms
  request_duration.record(total_duration, Some([
    ("http.method", AttributeValue::string("GET")),
    ("http.target", AttributeValue::string("/api/users")),
    ("http.status_code", AttributeValue::int(200L))
  ]))
  
  logger.info("Request completed", Some([
    ("request.id", AttributeValue::string("req-12345")),
    ("http.status_code", AttributeValue::int(200L)),
    ("response.size", AttributeValue::int(response_body_size)),
    ("duration.ms", AttributeValue::int(150L))
  ]))
  
  // 减少活跃连接数
  active_connections.add(-1L, Some([("server.endpoint", AttributeValue::string("/api/users"))]))
  
  // 验证所有遥测数据
  assert_eq(request_span.name, "HTTP GET /api/users")
  assert_eq(request_span.kind, Server)
  assert_eq(request_span.attributes.length(), 6)
  
  assert_eq(db_span.name, "database.query")
  assert_eq(db_span.kind, Internal)
  assert_eq(db_span.attributes.length(), 3)
  
  assert_eq(cache_span.name, "cache.get")
  assert_eq(cache_span.kind, Internal)
  assert_eq(cache_span.attributes.length(), 3)
}

test "microservice_communication_scenario" {
  // 模拟微服务间通信的遥测场景
  
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let logger_provider = NoopLoggerProvider::{}
  let meter_provider = NoopMeterProvider::{}
  
  let tracer = tracer_provider.get_tracer("order-service", Some("2.1.0"))
  let logger = logger_provider.get_logger("order-service", Some("2.1.0"))
  let meter = meter_provider.get_meter("order-service", Some("2.1.0"))
  
  // 创建传播器用于服务间上下文传播
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 开始订单处理
  let (order_ctx, order_span) = tracer.start_span(ctx, "order.process", Server, Some([
    ("service.name", AttributeValue::string("order-service")),
    ("operation.type", AttributeValue::string("order_processing")),
    ("order.id", AttributeValue::string("order-12345")),
    ("user.id", AttributeValue::string("user-67890"))
  ]))
  
  let order_counter = meter.create_counter("orders_processed_total", Some("orders"), Some("Total orders processed"))
  order_counter.add(1L, Some([
    ("order.type", AttributeValue::string("online_purchase")),
    ("payment.method", AttributeValue::string("credit_card"))
  ]))
  
  logger.info("Order processing started", Some([
    ("order.id", AttributeValue::string("order-12345")),
    ("user.id", AttributeValue::string("user-67890")),
    ("order.amount", AttributeValue::float(99.99))
  ]))
  
  // 模拟调用支付服务
  let payment_carrier = MapCarrier::new()
  composite_propagator.inject(order_ctx, payment_carrier)
  
  let (payment_ctx, payment_span) = tracer.start_span(order_ctx, "payment.service.call", Client, Some([
    ("peer.service", AttributeValue::string("payment-service")),
    ("http.method", AttributeValue::string("POST")),
    ("http.target", AttributeValue::string("/api/v1/payments")),
    ("payment.amount", AttributeValue::float(99.99)),
    ("payment.currency", AttributeValue::string("USD"))
  ]))
  
  let payment_client = meter.create_counter("payment_service_calls_total", Some("calls"), Some("Payment service calls"))
  payment_client.add(1L, Some([
    ("peer.service", AttributeValue::string("payment-service")),
    ("http.method", AttributeValue::string("POST")),
    ("http.status_code", AttributeValue::int(200L))
  ]))
  
  logger.info("Payment service call", Some([
    ("payment.id", AttributeValue::string("pay-abcdef")),
    ("payment.amount", AttributeValue::float(99.99)),
    ("payment.method", AttributeValue::string("credit_card"))
  ]))
  
  // 模拟调用库存服务
  let inventory_carrier = MapCarrier::new()
  composite_propagator.inject(order_ctx, inventory_carrier)
  
  let (inventory_ctx, inventory_span) = tracer.start_span(order_ctx, "inventory.service.call", Client, Some([
    ("peer.service", AttributeValue::string("inventory-service")),
    ("http.method", AttributeValue::string("PUT")),
    ("http.target", AttributeValue::string("/api/v1/inventory/reserve")),
    ("product.id", AttributeValue::string("prod-11111")),
    ("quantity", AttributeValue::int(2L))
  ]))
  
  let inventory_client = meter.create_counter("inventory_service_calls_total", Some("calls"), Some("Inventory service calls"))
  inventory_client.add(1L, Some([
    ("peer.service", AttributeValue::string("inventory-service")),
    ("http.method", AttributeValue::string("PUT")),
    ("http.status_code", AttributeValue::int(200L))
  ]))
  
  logger.info("Inventory service call", Some([
    ("product.id", AttributeValue::string("prod-11111")),
    ("quantity.reserved", AttributeValue::int(2L)),
    ("inventory.available", AttributeValue::int(50L))
  ]))
  
  // 模拟调用通知服务
  let notification_carrier = MapCarrier::new()
  composite_propagator.inject(order_ctx, notification_carrier)
  
  let (notification_ctx, notification_span) = tracer.start_span(order_ctx, "notification.service.call", Client, Some([
    ("peer.service", AttributeValue::string("notification-service")),
    ("http.method", AttributeValue::string("POST")),
    ("http.target", AttributeValue::string("/api/v1/notifications")),
    ("notification.type", AttributeValue::string("email")),
    ("recipient", AttributeValue::string("user@example.com"))
  ]))
  
  let notification_client = meter.create_counter("notification_service_calls_total", Some("calls"), Some("Notification service calls"))
  notification_client.add(1L, Some([
    ("peer.service", AttributeValue::string("notification-service")),
    ("notification.type", AttributeValue::string("email")),
    ("notification.status", AttributeValue::string("sent"))
  ]))
  
  logger.info("Notification sent", Some([
    ("notification.id", AttributeValue::string("notif-12345")),
    ("notification.type", AttributeValue::string("email")),
    ("recipient", AttributeValue::string("user@example.com"))
  ]))
  
  // 完成订单处理
  let order_duration = meter.create_histogram("order_processing_duration_seconds", Some("seconds"), Some("Order processing duration"))
  order_duration.record(2.5, Some([
    ("order.type", AttributeValue::string("online_purchase")),
    ("payment.method", AttributeValue::string("credit_card"))
  ]))
  
  logger.info("Order processing completed", Some([
    ("order.id", AttributeValue::string("order-12345")),
    ("order.status", AttributeValue::string("completed")),
    ("duration.ms", AttributeValue::int(2500L))
  ]))
  
  // 验证所有遥测数据
  assert_eq(order_span.name, "order.process")
  assert_eq(order_span.attributes.length(), 4)
  
  assert_eq(payment_span.name, "payment.service.call")
  assert_eq(payment_span.kind, Client)
  assert_eq(payment_span.attributes.length(), 6)
  
  assert_eq(inventory_span.name, "inventory.service.call")
  assert_eq(inventory_span.kind, Client)
  assert_eq(inventory_span.attributes.length(), 6)
  
  assert_eq(notification_span.name, "notification.service.call")
  assert_eq(notification_span.kind, Client)
  assert_eq(notification_span.attributes.length(), 6)
}

test "batch_processing_telemetry_scenario" {
  // 模拟批处理作业的遥测场景
  
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let logger_provider = NoopLoggerProvider::{}
  let meter_provider = NoopMeterProvider::{}
  
  let tracer = tracer_provider.get_tracer("batch-processor", Some("1.5.0"))
  let logger = logger_provider.get_logger("batch-processor", Some("1.5.0"))
  let meter = meter_provider.get_meter("batch-processor", Some("1.5.0"))
  
  // 创建批处理相关的指标
  let batch_counter = meter.create_counter("batch_jobs_total", Some("jobs"), Some("Total batch jobs"))
  let batch_duration = meter.create_histogram("batch_job_duration_seconds", Some("seconds"), Some("Batch job duration"))
  let records_processed = meter.create_counter("batch_records_processed_total", Some("records"), Some("Total records processed"))
  let error_counter = meter.create_counter("batch_errors_total", Some("errors"), Some("Total batch errors"))
  let memory_usage = meter.create_gauge("batch_memory_usage_bytes", Some("bytes"), Some("Batch job memory usage"))
  
  // 开始批处理作业
  let (batch_ctx, batch_span) = tracer.start_span(ctx, "batch.data.processing", Internal, Some([
    ("job.name", AttributeValue::string("daily_user_sync")),
    ("job.type", AttributeValue::string("data_synchronization")),
    ("batch.id", AttributeValue::string("batch-20220101-001")),
    ("source.system", AttributeValue::string("legacy_database")),
    ("target.system", AttributeValue::string("new_data_warehouse"))
  ]))
  
  batch_counter.add(1L, Some([
    ("job.name", AttributeValue::string("daily_user_sync")),
    ("job.type", AttributeValue::string("data_synchronization"))
  ]))
  
  logger.info("Batch job started", Some([
    ("job.id", AttributeValue::string("batch-20220101-001")),
    ("job.name", AttributeValue::string("daily_user_sync")),
    ("estimated.records", AttributeValue::int(10000L))
  ]))
  
  // 模拟数据读取阶段
  let (read_ctx, read_span) = tracer.start_span(batch_ctx, "batch.data.read", Internal, Some([
    ("operation.type", AttributeValue::string("data_extraction")),
    ("source.system", AttributeValue::string("legacy_database")),
    ("table.name", AttributeValue::string("users"))
  ]))
  
  let read_duration = meter.create_histogram("batch_read_duration_seconds", Some("seconds"), Some("Batch data read duration"))
  read_duration.record(30.5, Some([
    ("source.system", AttributeValue::string("legacy_database")),
    ("table.name", AttributeValue::string("users"))
  ]))
  
  logger.info("Data reading completed", Some([
    ("records.read", AttributeValue::int(10000L)),
    ("read.duration.ms", AttributeValue::int(30500L))
  ]))
  
  // 模拟数据转换阶段
  let (transform_ctx, transform_span) = tracer.start_span(batch_ctx, "batch.data.transform", Internal, Some([
    ("operation.type", AttributeValue::string("data_transformation")),
    ("transformation.rules", AttributeValue::int(5L)),
    ("records.processed", AttributeValue::int(10000L))
  ]))
  
  let transform_duration = meter.create_histogram("batch_transform_duration_seconds", Some("seconds"), Some("Batch data transform duration"))
  transform_duration.record(120.0, Some([
    ("transformation.rules", AttributeValue::int(5L))
  ]))
  
  records_processed.add(10000L, Some([
    ("operation.type", AttributeValue::string("data_transformation")),
    ("job.name", AttributeValue::string("daily_user_sync"))
  ]))
  
  // 模拟一些转换错误
  error_counter.add(15L, Some([
    ("operation.type", AttributeValue::string("data_transformation")),
    ("error.type", AttributeValue::string("validation_error")),
    ("job.name", AttributeValue::string("daily_user_sync"))
  ]))
  
  logger.warn("Data transformation completed with errors", Some([
    ("records.processed", AttributeValue::int(10000L)),
    ("records.failed", AttributeValue::int(15L)),
    ("transform.duration.ms", AttributeValue::int(120000L))
  ]))
  
  // 模拟数据写入阶段
  let (write_ctx, write_span) = tracer.start_span(batch_ctx, "batch.data.write", Internal, Some([
    ("operation.type", AttributeValue::string("data_load")),
    ("target.system", AttributeValue::string("new_data_warehouse")),
    ("table.name", AttributeValue::string("dim_users"))
  ]))
  
  let write_duration = meter.create_histogram("batch_write_duration_seconds", Some("seconds"), Some("Batch data write duration"))
  write_duration.record(45.2, Some([
    ("target.system", AttributeValue::string("new_data_warehouse")),
    ("table.name", AttributeValue::string("dim_users"))
  ]))
  
  logger.info("Data writing completed", Some([
    ("records.written", AttributeValue::int(9985L)),
    ("write.duration.ms", AttributeValue::int(45200L))
  ]))
  
  // 监控内存使用
  memory_usage.record(2147483648.0, Some([  // 2GB
    ("job.name", AttributeValue::string("daily_user_sync")),
    ("operation.phase", AttributeValue::string("transformation"))
  ]))
  
  // 完成批处理作业
  let total_duration = 195.7  // 总计约3.25分钟
  batch_duration.record(total_duration, Some([
    ("job.name", AttributeValue::string("daily_user_sync")),
    ("job.type", AttributeValue::string("data_synchronization"))
  ]))
  
  logger.info("Batch job completed", Some([
    ("job.id", AttributeValue::string("batch-20220101-001")),
    ("job.status", AttributeValue::string("completed")),
    ("records.total", AttributeValue::int(10000L)),
    ("records.success", AttributeValue::int(9985L)),
    ("records.failed", AttributeValue::int(15L)),
    ("duration.ms", AttributeValue::int(195700L))
  ]))
  
  // 验证所有遥测数据
  assert_eq(batch_span.name, "batch.data.processing")
  assert_eq(batch_span.attributes.length(), 6)
  
  assert_eq(read_span.name, "batch.data.read")
  assert_eq(read_span.attributes.length(), 3)
  
  assert_eq(transform_span.name, "batch.data.transform")
  assert_eq(transform_span.attributes.length(), 4)
  
  assert_eq(write_span.name, "batch.data.write")
  assert_eq(write_span.attributes.length(), 4)
}