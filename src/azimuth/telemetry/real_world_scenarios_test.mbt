// Azimuth Telemetry - Real World Scenarios Tests
// 真实世界场景测试

test "real_world_ecommerce_checkout_flow" {
  // 测试电子商务结账流程的遥测数据
  let base_context = @azimuth.telemetry.api.context.Context::empty()
  let base_time = 1640995200000000000L
  
  // 设置用户上下文
  let user_context = base_context
    .with_value(@azimuth.telemetry.api.context.create_key("user.id"), "user-12345")
    .with_value(@azimuth.telemetry.api.context.create_key("session.id"), "session-67890")
    .with_value(@azimuth.telemetry.api.context.create_key("cart.id"), "cart-abcde")
  
  // 创建结账流程的根Span
  let (checkout_context, checkout_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    user_context,
    "checkout.process",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("checkout-service")),
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("business")),
      ("cart.total", @azimuth.telemetry.api.common.AttributeValue::float(299.99)),
      ("currency", @azimuth.telemetry.api.common.AttributeValue::string("USD"))
    ]),
    Some(base_time)
  )
  
  // 库存检查Span
  let (inventory_context, inventory_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    checkout_context,
    "inventory.check",
    Some(@azimuth.telemetry.api.trace.Client),
    Some([
      ("service.target", @azimuth.telemetry.api.common.AttributeValue::string("inventory-service")),
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("external")),
      ("items.count", @azimuth.telemetry.api.common.AttributeValue::int(5L))
    ]),
    Some(base_time + 1000000L)
  )
  
  // 支付处理Span
  let (payment_context, payment_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    checkout_context,
    "payment.process",
    Some(@azimuth.telemetry.api.trace.Client),
    Some([
      ("service.target", @azimuth.telemetry.api.common.AttributeValue::string("payment-gateway")),
      ("payment.method", @azimuth.telemetry.api.common.AttributeValue::string("credit_card")),
      ("payment.amount", @azimuth.telemetry.api.common.AttributeValue::float(299.99)),
      ("payment.currency", @azimuth.telemetry.api.common.AttributeValue::string("USD"))
    ]),
    Some(base_time + 5000000L)
  )
  
  // 创建结账流程的日志记录
  let checkout_start_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(base_time)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Checkout process started for user user-12345")
    .with_attribute("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user-12345"))
    .with_attribute("session.id", @azimuth.telemetry.api.common.AttributeValue::string("session-67890"))
    .with_attribute("cart.id", @azimuth.telemetry.api.common.AttributeValue::string("cart-abcde"))
    .with_attribute("cart.total", @azimuth.telemetry.api.common.AttributeValue::float(299.99))
    .build()
  
  let payment_success_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(base_time + 8000000L)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Payment processed successfully")
    .with_attribute("payment.id", @azimuth.telemetry.api.common.AttributeValue::string("pay-789xyz"))
    .with_attribute("payment.amount", @azimuth.telemetry.api.common.AttributeValue::float(299.99))
    .with_attribute("payment.method", @azimuth.telemetry.api.common.AttributeValue::string("credit_card"))
    .build()
  
  // 创建业务指标
  let meter = @azimuth.telemetry.api.metrics.NoopMeter
  let checkout_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("checkouts.total", "checkouts", "Total checkouts")
  let revenue_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("revenue.total", "USD", "Total revenue")
  let checkout_duration = @azimuth.telemetry.api.metrics.NoopMeter::create_histogram("checkout.duration", "ms", "Checkout duration")
  
  // 记录业务指标
  let checkout_attrs = [
    ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user-12345")),
    ("payment.method", @azimuth.telemetry.api.common.AttributeValue::string("credit_card")),
    ("currency", @azimuth.telemetry.api.common.AttributeValue::string("USD"))
  ]
  
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some(checkout_attrs))
  @azimuth.telemetry.api.metrics.NoopCounter::add(29999, Some(checkout_attrs)) // 以分为单位
  @azimuth.telemetry.api.metrics.NoopHistogram::record(12000.0, Some(checkout_attrs)) // 12秒
  
  // 验证电子商务结账流程的遥测数据
  assert_eq(checkout_span.name, "checkout.process")
  assert_eq(inventory_span.name, "inventory.check")
  assert_eq(payment_span.name, "payment.process")
  
  assert_eq(checkout_start_log.body, Some("Checkout process started for user user-12345"))
  assert_eq(payment_success_log.body, Some("Payment processed successfully"))
}

test "real_world_microservice_request_flow" {
  // 测试微服务请求流程的遥测数据
  let base_context = @azimuth.telemetry.api.context.Context::empty()
  let base_time = 1640995200000000000L
  
  // API网关入口Span
  let (gateway_context, gateway_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    base_context,
    "api.gateway.request",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("api-gateway")),
      ("http.method", @azimuth.telemetry.api.common.AttributeValue::string("GET")),
      ("http.url", @azimuth.telemetry.api.common.AttributeValue::string("/api/v1/users/12345/profile")),
      ("http.scheme", @azimuth.telemetry.api.common.AttributeValue::string("https")),
      ("http.host", @azimuth.telemetry.api.common.AttributeValue::string("api.example.com")),
      ("user.agent", @azimuth.telemetry.api.common.AttributeValue::string("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")),
      ("client.ip", @azimuth.telemetry.api.common.AttributeValue::string("192.168.1.100"))
    ]),
    Some(base_time)
  )
  
  // 用户服务调用Span
  let (user_service_context, user_service_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    gateway_context,
    "user.service.call",
    Some(@azimuth.telemetry.api.trace.Client),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("user-service")),
      ("service.target", @azimuth.telemetry.api.common.AttributeValue::string("user-service:8080")),
      ("rpc.system", @azimuth.telemetry.api.common.AttributeValue::string("grpc")),
      ("rpc.method", @azimuth.telemetry.api.common.AttributeValue::string("GetUserProfile")),
      ("rpc.service", @azimuth.telemetry.api.common.AttributeValue::string("UserService"))
    ]),
    Some(base_time + 2000000L)
  )
  
  // 数据库查询Span
  let (db_context, db_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    user_service_context,
    "database.query",
    Some(@azimuth.telemetry.api.trace.Client),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("user-service")),
      ("db.system", @azimuth.telemetry.api.common.AttributeValue::string("postgresql")),
      ("db.name", @azimuth.telemetry.api.common.AttributeValue::string("userdb")),
      ("db.operation", @azimuth.telemetry.api.common.AttributeValue::string("SELECT")),
      ("db.statement", @azimuth.telemetry.api.common.AttributeValue::string("SELECT id, name, email FROM users WHERE id = $1"))
    ]),
    Some(base_time + 3000000L)
  )
  
  // 创建微服务请求的日志记录
  let gateway_request_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(base_time)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("API Gateway: Incoming GET /api/v1/users/12345/profile")
    .with_attribute("http.method", @azimuth.telemetry.api.common.AttributeValue::string("GET"))
    .with_attribute("http.url", @azimuth.telemetry.api.common.AttributeValue::string("/api/v1/users/12345/profile"))
    .with_attribute("client.ip", @azimuth.telemetry.api.common.AttributeValue::string("192.168.1.100"))
    .with_attribute("request.id", @azimuth.telemetry.api.common.AttributeValue::string("req-abc123"))
    .build()
  
  let db_query_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(base_time + 3000000L)
    .severity(@azimuth.telemetry.api.logs.Debug)
    .body("Database query executed: SELECT id, name, email FROM users WHERE id = $1")
    .with_attribute("db.system", @azimuth.telemetry.api.common.AttributeValue::string("postgresql"))
    .with_attribute("db.operation", @azimuth.telemetry.api.common.AttributeValue::string("SELECT"))
    .with_attribute("db.duration", @azimuth.telemetry.api.common.AttributeValue::float(25.5))
    .with_attribute("db.rows", @azimuth.telemetry.api.common.AttributeValue::int(1L))
    .build()
  
  // 创建微服务指标
  let meter = @azimuth.telemetry.api.metrics.NoopMeter
  let request_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("http.requests.total", "requests", "Total HTTP requests")
  let request_duration = @azimuth.telemetry.api.metrics.NoopMeter::create_histogram("http.request.duration", "ms", "HTTP request duration")
  let db_query_duration = @azimuth.telemetry.api.metrics.NoopMeter::create_histogram("db.query.duration", "ms", "Database query duration")
  
  // 记录微服务指标
  let http_attrs = [
    ("http.method", @azimuth.telemetry.api.common.AttributeValue::string("GET")),
    ("http.route", @azimuth.telemetry.api.common.AttributeValue::string("/api/v1/users/{id}/profile")),
    ("http.status_code", @azimuth.telemetry.api.common.AttributeValue::int(200L))
  ]
  
  let db_attrs = [
    ("db.system", @azimuth.telemetry.api.common.AttributeValue::string("postgresql")),
    ("db.operation", @azimuth.telemetry.api.common.AttributeValue::string("SELECT"))
  ]
  
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some(http_attrs))
  @azimuth.telemetry.api.metrics.NoopHistogram::record(150.0, Some(http_attrs))
  @azimuth.telemetry.api.metrics.NoopHistogram::record(25.5, Some(db_attrs))
  
  // 验证微服务请求流程的遥测数据
  assert_eq(gateway_span.name, "api.gateway.request")
  assert_eq(user_service_span.name, "user.service.call")
  assert_eq(db_span.name, "database.query")
  
  assert_eq(gateway_request_log.body, Some("API Gateway: Incoming GET /api/v1/users/12345/profile"))
  assert_eq(db_query_log.body, Some("Database query executed: SELECT id, name, email FROM users WHERE id = $1"))
}

test "real_world_iot_device_telemetry" {
  // 测试IoT设备遥测数据处理
  let base_context = @azimuth.telemetry.api.context.Context::empty()
  let base_time = 1640995200000000000L
  
  // 设置设备上下文
  let device_context = base_context
    .with_value(@azimuth.telemetry.api.context.create_key("device.id"), "device-sensor-001")
    .with_value(@azimuth.telemetry.api.context.create_key("device.type"), "temperature_sensor")
    .with_value(@azimuth.telemetry.api.context.create_key("location"), "warehouse-a")
    .with_value(@azimuth.telemetry.api.context.create_key("firmware.version"), "2.1.5")
  
  // 设备数据处理Span
  let (processing_context, processing_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    device_context,
    "iot.data.processing",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("iot-processor")),
      ("device.id", @azimuth.telemetry.api.common.AttributeValue::string("device-sensor-001")),
      ("device.type", @azimuth.telemetry.api.common.AttributeValue::string("temperature_sensor")),
      ("message.count", @azimuth.telemetry.api.common.AttributeValue::int(100L)),
      ("batch.id", @azimuth.telemetry.api.common.AttributeValue::string("batch-20220101-001"))
    ]),
    Some(base_time)
  )
  
  // 数据验证Span
  let (validation_context, validation_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    processing_context,
    "data.validation",
    Some(@azimuth.telemetry.api.trace.Internal),
    Some([
      ("validation.type", @azimuth.telemetry.api.common.AttributeValue::string("schema")),
      ("records.total", @azimuth.telemetry.api.common.AttributeValue::int(100L)),
      ("records.valid", @azimuth.telemetry.api.common.AttributeValue::int(95L)),
      ("records.invalid", @azimuth.telemetry.api.common.AttributeValue::int(5L))
    ]),
    Some(base_time + 1000000L)
  )
  
  // 创建IoT数据处理的日志记录
  let data_ingestion_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(base_time)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("IoT data ingestion started for device-sensor-001")
    .with_attribute("device.id", @azimuth.telemetry.api.common.AttributeValue::string("device-sensor-001"))
    .with_attribute("device.type", @azimuth.telemetry.api.common.AttributeValue::string("temperature_sensor"))
    .with_attribute("message.count", @azimuth.telemetry.api.common.AttributeValue::int(100L))
    .with_attribute("batch.id", @azimuth.telemetry.api.common.AttributeValue::string("batch-20220101-001"))
    .build()
  
  let validation_warning_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(base_time + 2000000L)
    .severity(@azimuth.telemetry.api.logs.Warn)
    .body("Data validation found 5 invalid records out of 100")
    .with_attribute("validation.type", @azimuth.telemetry.api.common.AttributeValue::string("schema"))
    .with_attribute("records.total", @azimuth.telemetry.api.common.AttributeValue::int(100L))
    .with_attribute("records.invalid", @azimuth.telemetry.api.common.AttributeValue::int(5L))
    .with_attribute("error.rate", @azimuth.telemetry.api.common.AttributeValue::float(0.05))
    .build()
  
  // 创建IoT指标
  let meter = @azimuth.telemetry.api.metrics.NoopMeter
  let device_message_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("device.messages.total", "messages", "Total device messages")
  let temperature_gauge = @azimuth.telemetry.api.metrics.NoopMeter::create_gauge("device.temperature", "celsius", "Device temperature")
  let validation_error_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("validation.errors.total", "errors", "Total validation errors")
  
  // 记录IoT指标
  let device_attrs = [
    ("device.id", @azimuth.telemetry.api.common.AttributeValue::string("device-sensor-001")),
    ("device.type", @azimuth.telemetry.api.common.AttributeValue::string("temperature_sensor")),
    ("location", @azimuth.telemetry.api.common.AttributeValue::string("warehouse-a"))
  ]
  
  @azimuth.telemetry.api.metrics.NoopCounter::add(100L, Some(device_attrs))
  @azimuth.telemetry.api.metrics.NoopGauge::record(48.5, Some(device_attrs))
  @azimuth.telemetry.api.metrics.NoopCounter::add(5L, Some(device_attrs))
  
  // 验证IoT设备遥测数据
  assert_eq(processing_span.name, "iot.data.processing")
  assert_eq(validation_span.name, "data.validation")
  
  assert_eq(data_ingestion_log.body, Some("IoT data ingestion started for device-sensor-001"))
  assert_eq(validation_warning_log.body, Some("Data validation found 5 invalid records out of 100"))
}