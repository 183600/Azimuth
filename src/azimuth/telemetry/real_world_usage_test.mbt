// Azimuth Telemetry - Real World Usage Test
// 测试真实世界使用场景

test "web_request_tracing_scenario" {
  // 模拟Web请求追踪场景
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("web_server")
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("web_server")
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("web_server")
  
  // 创建指标
  let request_counter = meter.create_counter("http_requests_total", Some("requests"), Some("Total HTTP requests"))
  let response_histogram = meter.create_histogram("http_response_time_seconds", Some("seconds"), Some("HTTP response time"))
  
  // 开始请求追踪
  let ctx = context::Context::empty()
  let request_attrs = [
    ("http.method", AttributeValue::string("POST")),
    ("http.url", AttributeValue::string("/api/v1/users")),
    ("http.user_agent", AttributeValue::string("Mozilla/5.0")),
    ("net.peer.ip", AttributeValue::string("192.168.1.100"))
  ]
  
  let (ctx_with_span, request_span) = tracer.start_span(
    ctx,
    "HTTP POST /api/v1/users",
    trace::Server,
    request_attrs
  )
  
  // 记录请求开始日志
  logger.info("Request started", [
    ("span.name", AttributeValue::string(request_span.name)),
    ("http.method", AttributeValue::string("POST")),
    ("http.url", AttributeValue::string("/api/v1/users"))
  ])
  
  // 模拟数据库操作
  let (ctx_with_db, db_span) = tracer.start_span(
    ctx_with_span,
    "database.query",
    trace::Internal,
    [
      ("db.system", AttributeValue::string("postgresql")),
      ("db.statement", AttributeValue::string("SELECT * FROM users WHERE id = $1")),
      ("db.operation", AttributeValue::string("SELECT"))
    ]
  )
  
  logger.debug("Database query executed", [
    ("db.operation", AttributeValue::string("SELECT")),
    ("db.duration_ms", AttributeValue::int(25))
  ])
  
  // 模拟外部API调用
  let (ctx_with_external, external_span) = tracer.start_span(
    ctx_with_db,
    "external.api.call",
    trace::Client,
    [
      ("http.url", AttributeValue::string("https://api.payment.com/charge")),
      ("http.method", AttributeValue::string("POST")),
      ("net.peer.name", AttributeValue::string("api.payment.com"))
    ]
  )
  
  logger.info("External API call completed", [
    ("external.service", AttributeValue::string("payment_api")),
    ("http.status_code", AttributeValue::int(200))
  ])
  
  // 记录请求指标
  request_counter.add(1, [
    ("http.method", AttributeValue::string("POST")),
    ("http.status_code", AttributeValue::int(201)),
    ("http.route", AttributeValue::string("/api/v1/users"))
  ])
  
  response_histogram.record(0.150, [
    ("http.method", AttributeValue::string("POST")),
    ("http.route", AttributeValue::string("/api/v1/users"))
  ])
  
  // 记录请求完成日志
  logger.info("Request completed", [
    ("http.status_code", AttributeValue::int(201)),
    ("response.size_bytes", AttributeValue::int(1024)),
    ("duration_ms", AttributeValue::int(150))
  ])
  
  // 验证所有操作都正确执行
  assert_eq(request_span.name, "HTTP POST /api/v1/users")
  assert_eq(db_span.name, "database.query")
  assert_eq(external_span.name, "external.api.call")
}

test "microservice_communication_scenario" {
  // 模拟微服务通信场景
  let ctx = context::Context::empty()
  
  // 设置初始上下文信息
  let ctx_with_correlation = ctx
    .with_value(context::create_key("correlation_id"), "corr-12345")
    .with_value(context::create_key("user_id"), "user-67890")
    .with_value(context::create_key("request_source"), "mobile_app")
  
  // 创建传播器
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  let baggage_propagator = propagation::W3CBaggagePropagator::{}
  let composite_propagator = propagation::CompositePropagator::new([
    trace_propagator,
    baggage_propagator
  ])
  
  // 模拟服务A到服务B的调用
  let carrier_ab = propagation::MapCarrier::new()
  composite_propagator.inject(ctx_with_correlation, carrier_ab)
  
  // 服务B接收并提取上下文
  let ctx_b = composite_propagator.extract(context::Context::empty(), carrier_ab)
  let ctx_b_enhanced = ctx_b
    .with_value(context::create_key("service_name"), "service_b")
    .with_value(context::create_key("operation"), "process_data")
  
  // 服务B调用服务C
  let carrier_bc = propagation::MapCarrier::new()
  composite_propagator.inject(ctx_b_enhanced, carrier_bc)
  
  // 服务C接收并处理
  let ctx_c = composite_propagator.extract(context::Context::empty(), carrier_bc)
  let ctx_c_final = ctx_c
    .with_value(context::create_key("service_name"), "service_c")
    .with_value(context::create_key("operation"), "store_data")
  
  // 验证上下文传播
  let correlation_id = ctx_c_final.get(context::create_key("correlation_id"))
  let user_id = ctx_c_final.get(context::create_key("user_id"))
  let request_source = ctx_c_final.get(context::create_key("request_source"))
  
  assert_eq(correlation_id, Some("corr-12345"))
  assert_eq(user_id, Some("user-67890"))
  assert_eq(request_source, Some("mobile_app"))
}

test "batch_processing_scenario" {
  // 模拟批处理场景
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("batch_processor")
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("batch_processor")
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("batch_processor")
  
  // 创建批处理指标
  let batch_counter = meter.create_counter("batches_processed_total", Some("batches"), Some("Total batches processed"))
  let item_counter = meter.create_counter("items_processed_total", Some("items"), Some("Total items processed"))
  let processing_histogram = meter.create_histogram("batch_processing_duration_seconds", Some("seconds"), Some("Batch processing duration"))
  
  // 开始批处理
  let (ctx_with_batch, batch_span) = tracer.start_span(
    context::Context::empty(),
    "batch.process",
    trace::Internal,
    [
      ("batch.id", AttributeValue::string("batch-001")),
      ("batch.size", AttributeValue::int(1000)),
      ("batch.type", AttributeValue::string("user_import"))
    ]
  )
  
  logger.info("Batch processing started", [
    ("batch.id", AttributeValue::string("batch-001")),
    ("batch.size", AttributeValue::int(1000))
  ])
  
  // 模拟处理批次中的项目
  let mut i = 0
  while i < 10 {  // 模拟处理10个子批次
    let (ctx_with_item, item_span) = tracer.start_span(
      ctx_with_batch,
      "item.process",
      trace::Internal,
      [
        ("item.index", AttributeValue::int(i * 100)),
        ("item.sub_batch_size", AttributeValue::int(100))
      ]
    )
    
    // 模拟项目处理
    if i % 3 == 0 {
      logger.warn("Item processing warning", [
        ("item.index", AttributeValue::int(i * 100)),
        ("warning.type", AttributeValue::string("duplicate_data"))
      ])
    }
    
    i = i + 1
  }
  
  // 记录批处理完成指标
  batch_counter.add(1, [
    ("batch.type", AttributeValue::string("user_import")),
    ("batch.status", AttributeValue::string("completed"))
  ])
  
  item_counter.add(1000, [
    ("batch.type", AttributeValue::string("user_import")),
    ("item.status", AttributeValue::string("processed"))
  ])
  
  processing_histogram.record(30.5, [
    ("batch.type", AttributeValue::string("user_import")),
    ("batch.size", AttributeValue::int(1000))
  ])
  
  logger.info("Batch processing completed", [
    ("batch.id", AttributeValue::string("batch-001")),
    ("items.processed", AttributeValue::int(1000)),
    ("duration_seconds", AttributeValue::float(30.5))
  ])
  
  // 验证批处理span
  assert_eq(batch_span.name, "batch.process")
  assert_eq(batch_span.attributes.length(), 3)
}

test "error_handling_scenario" {
  // 模拟错误处理场景
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("error_service")
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("error_service")
  
  let ctx = context::Context::empty()
  let (ctx_with_span, operation_span) = tracer.start_span(
    ctx,
    "risky_operation",
    trace::Internal,
    [("operation.type", AttributeValue::string("database_write"))]
  )
  
  // 模拟错误发生
  let (ctx_with_error, error_span) = tracer.start_span(
    ctx_with_span,
    "error.handling",
    trace::Internal,
    [
      ("error.type", AttributeValue::string("ConnectionTimeout")),
      ("error.retry_count", AttributeValue::int(3))
    ]
  )
  
  // 记录错误日志
  logger.error("Operation failed due to timeout", [
    ("error.type", AttributeValue::string("ConnectionTimeout")),
    ("error.message", AttributeValue::string("Database connection timeout after 30 seconds")),
    ("error.retry_count", AttributeValue::int(3)),
    ("operation.id", AttributeValue::string("op-12345"))
  ])
  
  // 模拟恢复操作
  let (ctx_with_recovery, recovery_span) = tracer.start_span(
    ctx_with_error,
    "recovery.operation",
    trace::Internal,
    [
      ("recovery.type", AttributeValue::string("fallback_database")),
      ("recovery.success", AttributeValue::bool(true))
    ]
  )
  
  logger.info("Recovery operation successful", [
    ("recovery.type", AttributeValue::string("fallback_database")),
    ("recovery.duration_ms", AttributeValue::int(5000))
  ])
  
  // 验证错误处理流程
  assert_eq(operation_span.name, "risky_operation")
  assert_eq(error_span.name, "error.handling")
  assert_eq(recovery_span.name, "recovery.operation")
  
  assert_eq(error_span.attributes.length(), 2)
  assert_eq(recovery_span.attributes.length(), 2)
}