// 实际使用场景测试 - 模拟真实世界的Web应用场景

test "web_request_lifecycle_telemetry" {
  // 模拟完整的Web请求生命周期，包括HTTP请求处理、数据库操作、缓存访问等
  
  // 1. HTTP请求接收
  let request_ctx = Context::empty()
    .with_value(create_key("request.id"), "req-web-001")
    .with_value(create_key("client.ip"), "203.0.113.1")
    .with_value(create_key("user.agent"), "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
    .with_value(create_key("request.start.time"), "1640995200000")
  
  let request_baggage = Baggage::empty()
    .with_entry("http.method", "GET")
    .with_entry("http.path", "/api/v1/products/123")
    .with_entry("http.scheme", "https")
    .with_entry("http.host", "api.example.com")
    .with_entry("user.id", "user-web-456")
    .with_entry("session.id", "sess-web-789")
  
  // 2. 创建主请求Span
  let (_, main_span) = NoopTracer::start_span(
    request_ctx,
    "http.request",
    Server,
    Some([
      ("http.method", AttributeValue::string("GET")),
      ("http.path", AttributeValue::string("/api/v1/products/123")),
      ("http.scheme", AttributeValue::string("https")),
      ("http.host", AttributeValue::string("api.example.com")),
      ("http.user_agent", AttributeValue::string("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")),
      ("client.ip", AttributeValue::string("203.0.113.1")),
      ("user.id", AttributeValue::string("user-web-456"))
    ])
  )
  
  // 3. 记录请求Metrics
  let web_meter = NoopMeter::{}
  let request_counter = web_meter.create_counter("http_requests_total", Some("requests"), Some("Total HTTP requests"))
  
  let request_attributes = [
    ("http.method", AttributeValue::string("GET")),
    ("http.path", AttributeValue::string("/api/v1/products/123")),
    ("http.status_code", AttributeValue::int(200L)),
    ("user.id", AttributeValue::string("user-web-456")),
    ("endpoint.type", AttributeValue::string("product_detail"))
  ]
  
  request_counter.add(1L, Some(request_attributes))
  
  // 4. 身份验证检查
  let auth_ctx = request_ctx.with_value(create_key("auth.required"), "false")
  let (_, auth_span) = NoopTracer::start_span(
    auth_ctx,
    "auth.check",
    Internal,
    Some([
      ("auth.type", AttributeValue::string("optional")),
      ("auth.result", AttributeValue::string("skipped")),
      ("endpoint", AttributeValue::string("public_product"))
    ])
  )
  
  // 5. 缓存检查
  let cache_ctx = auth_ctx.with_value(create_key("cache.key"), "product:123"))
  let (_, cache_span) = NoopTracer::start_span(
    cache_ctx,
    "cache.get",
    Internal,
    Some([
      ("cache.operation", AttributeValue::string("get")),
      ("cache.key", AttributeValue::string("product:123")),
      ("cache.backend", AttributeValue::string("redis"))
    ])
  )
  
  let cache_meter = NoopMeter::{}
  let cache_counter = cache_meter.create_counter("cache_operations_total", Some("operations"), Some("Total cache operations"))
  let cache_histogram = cache_meter.create_histogram("cache_duration_ms", Some("ms"), Some("Cache operation duration"))
  
  // 模拟缓存未命中
  cache_counter.add(1L, Some([
    ("cache.operation", AttributeValue::string("get")),
    ("cache.result", AttributeValue::string("miss")),
    ("cache.backend", AttributeValue::string("redis"))
  ]))
  
  cache_histogram.record(2.5, Some([
    ("cache.operation", AttributeValue::string("get")),
    ("cache.backend", AttributeValue::string("redis"))
  ]))
  
  // 6. 数据库查询
  let db_ctx = cache_ctx.with_value(create_key("db.query"), "SELECT * FROM products WHERE id = ?")
  let (_, db_span) = NoopTracer::start_span(
    db_ctx,
    "db.query",
    Internal,
    Some([
      ("db.operation", AttributeValue::string("select")),
      ("db.table", AttributeValue::string("products")),
      ("db.query.type", AttributeValue::string("single_row"))
    ])
  )
  
  let db_meter = NoopMeter::{}
  let db_counter = db_meter.create_counter("db_queries_total", Some("queries"), Some("Total database queries"))
  let db_histogram = db_meter.create_histogram("db_query_duration_ms", Some("ms"), Some("Database query duration"))
  
  db_counter.add(1L, Some([
    ("db.operation", AttributeValue::string("select")),
    ("db.table", AttributeValue::string("products")),
    ("db.result", AttributeValue::string("success"))
  ]))
  
  db_histogram.record(45.0, Some([
    ("db.operation", AttributeValue::string("select")),
    ("db.table", AttributeValue::string("products"))
  ]))
  
  // 7. 数据处理和序列化
  let processing_ctx = db_ctx.with_value(create_key("processing.type"), "json_serialization")
  let (_, processing_span) = NoopTracer::start_span(
    processing_ctx,
    "data.processing",
    Internal,
    Some([
      ("processing.operation", AttributeValue::string("serialize")),
      ("data.format", AttributeValue::string("json")),
      ("record.count", AttributeValue::int(1L))
    ])
  )
  
  // 8. 缓存写入
  let cache_write_ctx = processing_ctx.with_value(create_key("cache.ttl"), "300")
  let (_, cache_write_span) = NoopTracer::start_span(
    cache_write_ctx,
    "cache.set",
    Internal,
    Some([
      ("cache.operation", AttributeValue::string("set")),
      ("cache.key", AttributeValue::string("product:123")),
      ("cache.ttl", AttributeValue::int(300L)),
      ("cache.backend", AttributeValue::string("redis"))
    ])
  )
  
  cache_counter.add(1L, Some([
    ("cache.operation", AttributeValue::string("set")),
    ("cache.result", AttributeValue::string("success")),
    ("cache.backend", AttributeValue::string("redis"))
  ]))
  
  cache_histogram.record(1.8, Some([
    ("cache.operation", AttributeValue::string("set")),
    ("cache.backend", AttributeValue::string("redis"))
  ]))
  
  // 9. 响应发送
  let response_ctx = cache_write_ctx.with_value(create_key("response.status"), "200")
  let (_, response_span) = NoopTracer::start_span(
    response_ctx,
    "http.response",
    Internal,
    Some([
      ("http.status_code", AttributeValue::int(200L)),
      ("response.size", AttributeValue::int(1024L)),
      ("response.content_type", AttributeValue::string("application/json"))
    ])
  )
  
  let response_histogram = web_meter.create_histogram("http_response_duration_ms", Some("ms"), Some("HTTP response duration"))
  response_histogram.record(125.0, Some([
    ("http.method", AttributeValue::string("GET")),
    ("http.path", AttributeValue::string("/api/v1/products/123")),
    ("http.status_code", AttributeValue::int(200L))
  ]))
  
  // 10. 日志记录
  let web_logger = NoopLogger::{}
  
  web_logger.info("HTTP request processed successfully", Some([
    ("request.id", AttributeValue::string("req-web-001")),
    ("http.method", AttributeValue::string("GET")),
    ("http.path", AttributeValue::string("/api/v1/products/123")),
    ("http.status_code", AttributeValue::int(200L)),
    ("user.id", AttributeValue::string("user-web-456")),
    ("client.ip", AttributeValue::string("203.0.113.1")),
    ("duration.ms", AttributeValue::int(125L)),
    ("cache.hit", AttributeValue::bool(false)),
    ("db.queries", AttributeValue::int(1L))
  ]))
  
  // 11. 验证整个请求生命周期
  assert_eq(main_span.name, "http.request")
  assert_eq(auth_span.name, "auth.check")
  assert_eq(cache_span.name, "cache.get")
  assert_eq(db_span.name, "db.query")
  assert_eq(processing_span.name, "data.processing")
  assert_eq(cache_write_span.name, "cache.set")
  assert_eq(response_span.name, "http.response")
  
  // 验证Context传播
  match request_ctx.get(create_key("request.id")) {
    Some(value) => assert_eq(value, "req-web-001")
    None => @test.fail("Test failed")
  }
  
  match db_ctx.get(create_key("db.query")) {
    Some(value) => assert_eq(value, "SELECT * FROM products WHERE id = ?")
    None => @test.fail("Test failed")
  }
}

test "microservice_payment_workflow" {
  // 模拟微服务支付工作流，包括多个服务间的调用
  
  // 1. 支付请求初始化
  let payment_ctx = Context::empty()
    .with_value(create_key("payment.id"), "pay-ms-001")
    .with_value(create_key("correlation.id"), "corr-ms-12345")
    .with_value(create_key("user.id"), "user-ms-789")
    .with_value(create_key("merchant.id"), "merchant-ms-456")
  
  let payment_baggage = Baggage::empty()
    .with_entry("service.name", "payment-service")
    .with_entry("service.version", "3.2.1")
    .with_entry("payment.amount", "299.99")
    .with_entry("payment.currency", "USD")
    .with_entry("payment.method", "credit_card")
    .with_entry("request.source", "mobile_app")
  
  // 2. 主支付处理Span
  let (_, payment_span) = NoopTracer::start_span(
    payment_ctx,
    "payment.process",
    Server,
    Some([
      ("payment.id", AttributeValue::string("pay-ms-001")),
      ("correlation.id", AttributeValue::string("corr-ms-12345")),
      ("user.id", AttributeValue::string("user-ms-789")),
      ("merchant.id", AttributeValue::string("merchant-ms-456")),
      ("payment.amount", AttributeValue::float(299.99)),
      ("payment.currency", AttributeValue::string("USD")),
      ("payment.method", AttributeValue::string("credit_card"))
    ])
  )
  
  // 3. 风险评估服务调用
  let risk_ctx = payment_ctx.with_value(create_key("risk.score.required"), "true")
  let (_, risk_span) = NoopTracer::start_span(
    risk_ctx,
    "risk.assessment",
    Client,
    Some([
      ("service.name", AttributeValue::string("risk-service")),
      ("operation.type", AttributeValue::string("assess_transaction")),
      ("payment.amount", AttributeValue::float(299.99)),
      ("user.id", AttributeValue::string("user-ms-789"))
    ])
  )
  
  let risk_meter = NoopMeter::{}
  let risk_counter = risk_meter.create_counter("risk_assessments_total", Some("assessments"), Some("Total risk assessments"))
  let risk_histogram = risk_meter.create_histogram("risk_assessment_duration_ms", Some("ms"), Some("Risk assessment duration"))
  
  risk_counter.add(1L, Some([
    ("service.name", AttributeValue::string("risk-service")),
    ("risk.result", AttributeValue::string("approved")),
    ("risk.score", AttributeValue::int(25L)),
    ("user.risk.tier", AttributeValue::string("low"))
  ]))
  
  risk_histogram.record(150.0, Some([
    ("service.name", AttributeValue::string("risk-service")),
    ("assessment.type", AttributeValue::string("transaction"))
  ]))
  
  // 4. 银行网关调用
  let bank_ctx = risk_ctx.with_value(create_key("bank.gateway"), "acme_bank")
  let (_, bank_span) = NoopTracer::start_span(
    bank_ctx,
    "bank.authorization",
    Client,
    Some([
      ("service.name", AttributeValue::string("bank-gateway-service")),
      ("bank.name", AttributeValue::string("acme_bank")),
      ("operation.type", AttributeValue::string("authorize")),
      ("payment.amount", AttributeValue::float(299.99)),
      ("card.type", AttributeValue::string("credit"))
    ])
  )
  
  let bank_meter = NoopMeter::{}
  let bank_counter = bank_meter.create_counter("bank_authorizations_total", Some("authorizations"), Some("Total bank authorizations"))
  let bank_histogram = bank_meter.create_histogram("bank_authorization_duration_ms", Some("ms"), Some("Bank authorization duration"))
  
  bank_counter.add(1L, Some([
    ("service.name", AttributeValue::string("bank-gateway-service")),
    ("bank.name", AttributeValue::string("acme_bank")),
    ("auth.result", AttributeValue::string("approved")),
    ("auth.code", AttributeValue::string("00"))
  ]))
  
  bank_histogram.record(850.0, Some([
    ("service.name", AttributeValue::string("bank-gateway-service")),
    ("bank.name", AttributeValue::string("acme_bank"))
  ]))
  
  // 5. 余额更新服务调用
  let balance_ctx = bank_ctx.with_value(create_key("account.type"), "checking")
  let (_, balance_span) = NoopTracer::start_span(
    balance_ctx,
    "account.balance.update",
    Client,
    Some([
      ("service.name", AttributeValue::string("account-service")),
      ("operation.type", AttributeValue::string("debit")),
      ("account.type", AttributeValue::string("checking")),
      ("amount", AttributeValue::float(299.99))
    ])
  )
  
  let account_meter = NoopMeter::{}
  let account_counter = account_meter.create_counter("account_updates_total", Some("updates"), Some("Total account updates"))
  let account_gauge = account_meter.create_gauge("account_balance", Some("USD"), Some("Account balance"))
  
  account_counter.add(1L, Some([
    ("service.name", AttributeValue::string("account-service")),
    ("operation.type", AttributeValue::string("debit")),
    ("result", AttributeValue::string("success"))
  ]))
  
  account_gauge.record(1250.50, Some([
    ("service.name", AttributeValue::string("account-service")),
    ("account.type", AttributeValue::string("checking")),
    ("user.id", AttributeValue::string("user-ms-789"))
  ]))
  
  // 6. 通知服务调用
  let notification_ctx = balance_ctx.with_value(create_key("notification.type"), "payment_confirmation")
  let (_, notification_span) = NoopTracer::start_span(
    notification_ctx,
    "notification.send",
    Client,
    Some([
      ("service.name", AttributeValue::string("notification-service")),
      ("notification.type", AttributeValue::string("email")),
      ("notification.template", AttributeValue::string("payment_confirmation")),
      ("recipient", AttributeValue::string("user@example.com"))
    ])
  )
  
  let notification_meter = NoopMeter::{}
  let notification_counter = notification_meter.create_counter("notifications_sent_total", Some("notifications"), Some("Total notifications sent"))
  
  notification_counter.add(1L, Some([
    ("service.name", AttributeValue::string("notification-service")),
    ("notification.type", AttributeValue::string("email")),
    ("notification.status", AttributeValue::string("sent"))
  ]))
  
  // 7. 审计日志记录
  let audit_ctx = notification_ctx.with_value(create_key("audit.required"), "true")
  let (_, audit_span) = NoopTracer::start_span(
    audit_ctx,
    "audit.log",
    Internal,
    Some([
      ("service.name", AttributeValue::string("audit-service")),
      ("operation.type", AttributeValue::string("log_payment")),
      ("audit.category", AttributeValue::string("financial")),
      ("compliance.required", AttributeValue::bool(true))
    ])
  )
  
  // 8. 支付完成Metrics和日志
  let payment_meter = NoopMeter::{}
  let payment_success_counter = payment_meter.create_counter("payments_completed_total", Some("payments"), Some("Total completed payments"))
  let payment_amount_histogram = payment_meter.create_histogram("payment_amount_distribution", Some("USD"), Some("Payment amount distribution"))
  let payment_duration_histogram = payment_meter.create_histogram("payment_processing_duration_ms", Some("ms"), Some("Payment processing duration"))
  
  payment_success_counter.add(1L, Some([
    ("service.name", AttributeValue::string("payment-service")),
    ("payment.method", AttributeValue::string("credit_card")),
    ("payment.result", AttributeValue::string("success")),
    ("merchant.id", AttributeValue::string("merchant-ms-456"))
  ]))
  
  payment_amount_histogram.record(299.99, Some([
    ("service.name", AttributeValue::string("payment-service")),
    ("payment.method", AttributeValue::string("credit_card")),
    ("user.tier", AttributeValue::string("premium"))
  ]))
  
  payment_duration_histogram.record(1250.0, Some([
    ("service.name", AttributeValue::string("payment-service")),
    ("payment.method", AttributeValue::string("credit_card"))
  ]))
  
  let payment_logger = NoopLogger::{}
  payment_logger.info("Payment processed successfully", Some([
    ("payment.id", AttributeValue::string("pay-ms-001")),
    ("correlation.id", AttributeValue::string("corr-ms-12345")),
    ("user.id", AttributeValue::string("user-ms-789")),
    ("merchant.id", AttributeValue::string("merchant-ms-456")),
    ("payment.amount", AttributeValue::float(299.99)),
    ("payment.currency", AttributeValue::string("USD")),
    ("payment.method", AttributeValue::string("credit_card")),
    ("risk.score", AttributeValue::int(25L)),
    ("bank.auth.code", AttributeValue::string("00")),
    ("total.duration.ms", AttributeValue::int(1250L)),
    ("services.involved", AttributeValue::array_string(["payment-service", "risk-service", "bank-gateway-service", "account-service", "notification-service", "audit-service"]))
  ]))
  
  // 9. 验证整个支付工作流
  assert_eq(payment_span.name, "payment.process")
  assert_eq(risk_span.name, "risk.assessment")
  assert_eq(bank_span.name, "bank.authorization")
  assert_eq(balance_span.name, "account.balance.update")
  assert_eq(notification_span.name, "notification.send")
  assert_eq(audit_span.name, "audit.log")
  
  // 验证Context一致性
  match payment_ctx.get(create_key("payment.id")) {
    Some(value) => assert_eq(value, "pay-ms-001")
    None => @test.fail("Test failed")
  }
  
  match bank_ctx.get(create_key("bank.gateway")) {
    Some(value) => assert_eq(value, "acme_bank")
    None => @test.fail("Test failed")
  }
  
  match balance_ctx.get(create_key("account.type")) {
    Some(value) => assert_eq(value, "checking")
    None => @test.fail("Test failed")
  }
}

test "iot_device_data_processing" {
  // 模拟IoT设备数据处理场景
  
  // 1. 设备数据接收
  let iot_ctx = Context::empty()
    .with_value(create_key("device.id"), "device-iot-001")
    .with_value(create_key("device.type"), "temperature_sensor")
    .with_value(create_key("location"), "warehouse_a")
    .with_value(create_key("batch.id"), "batch-iot-123")
  
  let iot_baggage = Baggage::empty()
    .with_entry("device.manufacturer", "sensorcorp")
    .with_entry("device.model", "temp-pro-5000")
    .with_entry("firmware.version", "2.1.3")
    .with_entry("data.protocol", "mqtt")
    .with_entry("processing.priority", "normal")
  
  // 2. 数据接收Span
  let (_, receive_span) = NoopTracer::start_span(
    iot_ctx,
    "iot.data.receive",
    Server,
    Some([
      ("device.id", AttributeValue::string("device-iot-001")),
      ("device.type", AttributeValue::string("temperature_sensor")),
      ("location", AttributeValue::string("warehouse_a")),
      ("data.protocol", AttributeValue::string("mqtt")),
      ("message.size", AttributeValue::int(256L))
    ])
  )
  
  // 3. 数据验证
  let validation_ctx = iot_ctx.with_value(create_key("validation.rules"), "temperature_range")
  let (_, validation_span) = NoopTracer::start_span(
    validation_ctx,
    "data.validation",
    Internal,
    Some([
      ("validation.type", AttributeValue::string("schema")),
      ("validation.rules", AttributeValue::string("temperature_range")),
      ("data.type", AttributeValue::string("sensor_reading"))
    ])
  )
  
  let validation_meter = NoopMeter::{}
  let validation_counter = validation_meter.create_counter("data_validations_total", Some("validations"), Some("Total data validations"))
  
  validation_counter.add(1L, Some([
    ("service.name", AttributeValue::string("data-processing-service")),
    ("validation.result", AttributeValue::string("passed")),
    ("device.type", AttributeValue::string("temperature_sensor"))
  ]))
  
  // 4. 数据转换和标准化
  let transform_ctx = validation_ctx.with_value(create_key("transform.type"), "unit_conversion")
  let (_, transform_span) = NoopTracer::start_span(
    transform_ctx,
    "data.transformation",
    Internal,
    Some([
      ("transform.operation", AttributeValue::string("celsius_to_fahrenheit")),
      ("input.unit", AttributeValue::string("celsius")),
      ("output.unit", AttributeValue::string("fahrenheit"))
    ])
  )
  
  // 5. 异常检测
  let anomaly_ctx = transform_ctx.with_value(create_key("anomaly.detection"), "statistical")
  let (_, anomaly_span) = NoopTracer::start_span(
    anomaly_ctx,
    "anomaly.detection",
    Internal,
    Some([
      ("detection.method", AttributeValue::string("statistical")),
      ("algorithm", AttributeValue::string("z_score")),
      ("threshold", AttributeValue::float(2.5))
    ])
  )
  
  let anomaly_meter = NoopMeter::{}
  let anomaly_counter = anomaly_meter.create_counter("anomaly_detections_total", Some("detections"), Some("Total anomaly detections"))
  let anomaly_gauge = anomaly_meter.create_gauge("anomaly_score", Some("score"), Some("Current anomaly score"))
  
  anomaly_counter.add(1L, Some([
    ("service.name", AttributeValue::string("anomaly-detection-service")),
    ("detection.result", AttributeValue::string("normal")),
    ("anomaly.score", AttributeValue::float(0.8))
  ]))
  
  anomaly_gauge.record(0.8, Some([
    ("device.id", AttributeValue::string("device-iot-001")),
    ("location", AttributeValue::string("warehouse_a"))
  ]))
  
  // 6. 数据聚合
  let aggregation_ctx = anomaly_ctx.with_value(create_key("aggregation.window"), "5m")
  let (_, aggregation_span) = NoopTracer::start_span(
    aggregation_ctx,
    "data.aggregation",
    Internal,
    Some([
      ("aggregation.type", AttributeValue::string("time_window")),
      ("window.size", AttributeValue::string("5m")),
      ("aggregation.functions", AttributeValue::array_string(["avg", "min", "max", "count"]))
    ])
  )
  
  let aggregation_meter = NoopMeter::{}
  let aggregation_histogram = aggregation_meter.create_histogram("aggregation_duration_ms", Some("ms"), Some("Data aggregation duration"))
  
  aggregation_histogram.record(25.0, Some([
    ("service.name", AttributeValue::string("data-aggregation-service")),
    ("aggregation.type", AttributeValue::string("time_window")),
    ("window.size", AttributeValue::string("5m"))
  ]))
  
  // 7. 数据存储
  let storage_ctx = aggregation_ctx.with_value(create_key("storage.backend"), "timeseries_db")
  let (_, storage_span) = NoopTracer::start_span(
    storage_ctx,
    "data.storage",
    Internal,
    Some([
      ("storage.operation", AttributeValue::string("write")),
      ("storage.backend", AttributeValue::string("timeseries_db")),
      ("table.name", AttributeValue::string("sensor_readings"))
    ])
  )
  
  let storage_meter = NoopMeter::{}
  let storage_counter = storage_meter.create_counter("storage_operations_total", Some("operations"), Some("Total storage operations"))
  let storage_gauge = storage_meter.create_gauge("storage_size_mb", Some("MB"), Some("Storage size in MB"))
  
  storage_counter.add(1L, Some([
    ("service.name", AttributeValue::string("storage-service")),
    ("storage.operation", AttributeValue::string("write")),
    ("storage.backend", AttributeValue::string("timeseries_db")),
    ("result", AttributeValue::string("success"))
  ]))
  
  storage_gauge.record1024.0, Some([
    ("storage.backend", AttributeValue::string("timeseries_db")),
    ("location", AttributeValue::string("warehouse_a"))
  ]))
  
  // 8. 实时监控和告警
  let monitoring_ctx = storage_ctx.with_value(create_key("alert.threshold"), "temperature_high")
  let (_, monitoring_span) = NoopTracer::start_span(
    monitoring_ctx,
    "realtime.monitoring",
    Internal,
    Some([
      ("monitoring.type", AttributeValue::string("threshold_check")),
      ("alert.type", AttributeValue::string("temperature")),
      ("threshold.value", AttributeValue::float(35.0))
    ])
  )
  
  // 9. 数据处理完成Metrics和日志
  let iot_meter = NoopMeter::{}
  let processing_counter = iot_meter.create_counter("iot_data_processed_total", Some("records"), Some("Total IoT data records processed"))
  let processing_duration_histogram = iot_meter.create_histogram("iot_processing_duration_ms", Some("ms"), Some("IoT data processing duration"))
  
  processing_counter.add(1L, Some([
    ("service.name", AttributeValue::string("iot-processing-service")),
    ("device.type", AttributeValue::string("temperature_sensor")),
    ("location", AttributeValue::string("warehouse_a")),
    ("processing.result", AttributeValue::string("success"))
  ]))
  
  processing_duration_histogram.record(85.0, Some([
    ("service.name", AttributeValue::string("iot-processing-service")),
    ("device.type", AttributeValue::string("temperature_sensor"))
  ]))
  
  let iot_logger = NoopLogger::{}
  iot_logger.info("IoT data processed successfully", Some([
    ("device.id", AttributeValue::string("device-iot-001")),
    ("device.type", AttributeValue::string("temperature_sensor")),
    ("location", AttributeValue::string("warehouse_a")),
    ("batch.id", AttributeValue::string("batch-iot-123")),
    ("data.points", AttributeValue::int(1L)),
    ("temperature.celsius", AttributeValue::float(22.5)),
    ("temperature.fahrenheit", AttributeValue::float(72.5)),
    ("anomaly.score", AttributeValue::float(0.8)),
    ("processing.duration.ms", AttributeValue::int(85L)),
    ("storage.backend", AttributeValue::string("timeseries_db"))
  ]))
  
  // 10. 验证整个IoT数据处理流程
  assert_eq(receive_span.name, "iot.data.receive")
  assert_eq(validation_span.name, "data.validation")
  assert_eq(transform_span.name, "data.transformation")
  assert_eq(anomaly_span.name, "anomaly.detection")
  assert_eq(aggregation_span.name, "data.aggregation")
  assert_eq(storage_span.name, "data.storage")
  assert_eq(monitoring_span.name, "realtime.monitoring")
  
  // 验证Context传播
  match iot_ctx.get(create_key("device.id")) {
    Some(value) => assert_eq(value, "device-iot-001")
    None => @test.fail("Test failed")
  }
  
  match aggregation_ctx.get(create_key("aggregation.window")) {
    Some(value) => assert_eq(value, "5m")
    None => @test.fail("Test failed")
  }
}