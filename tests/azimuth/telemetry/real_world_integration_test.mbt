// 真实世界场景集成测试用例
// 模拟真实应用场景中的复杂遥测操作和集成测试

test "Web应用请求处理完整流程测试" {
  // 模拟Web应用的完整请求处理流程
  
  // 1. 初始化遥测系统
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "web-server", Some("1.0.0"))
  let meter = MeterProvider::get_meter(meter_provider, "web-server-metrics")
  let logger = LoggerProvider::get_logger(logger_provider, "web-server-logger")
  
  // 2. 创建请求级别的Span
  let request_span_ctx = SpanContext::new("web_trace_123", "web_span_456", true, "")
  let request_span = Span::new("HTTP GET /api/users", Server, request_span_ctx)
  
  // 3. 添加请求属性
  Span::add_event(request_span, "请求开始", Some([
    ("http.method", StringValue("GET")),
    ("http.url", StringValue("https://api.example.com/api/users?page=1&limit=10")),
    ("http.user_agent", StringValue("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")),
    ("http.remote_addr", StringValue("192.168.1.100")),
    ("user.id", StringValue("user_中文_123"))
  ]))
  
  // 4. 记录请求指标
  let request_counter = Meter::create_counter(meter, "http_requests_total", Some("HTTP请求总数"), Some("requests"))
  let request_duration = Meter::create_histogram(meter, "http_request_duration_ms", Some("HTTP请求持续时间"), Some("ms"))
  
  Counter::add(request_counter, 1.0, Some(Attributes::new()))
  Histogram::record(request_duration, 150.5, None)
  
  // 5. 数据库操作
  let db_span = Span::new("database.query", Client, SpanContext::new("web_trace_123", "db_span_789", true, ""))
  Span::add_event(db_span, "执行SQL查询", Some([
    ("db.statement", StringValue("SELECT * FROM users WHERE active = true LIMIT 10")),
    ("db.type", StringValue("postgresql")),
    ("db.instance", StringValue("prod-db-01"))
  ]))
  
  // 模拟数据库查询时间
  Histogram::record(request_duration, 25.3, None)
  Span::end(db_span)
  
  // 6. 缓存操作
  let cache_span = Span::new("cache.get", Client, SpanContext::new("web_trace_123", "cache_span_101", true, ""))
  Span::add_event(cache_span, "缓存查询", Some([
    ("cache.key", StringValue("users:page:1:active")),
    ("cache.hit", BoolValue(false)),
    ("cache.ttl", IntValue(300))
  ]))
  Span::end(cache_span)
  
  // 7. 响应处理
  Span::add_event(request_span, "生成响应", Some([
    ("http.status_code", IntValue(200)),
    ("response.size_bytes", IntValue(2048)),
    ("response.content_type", StringValue("application/json"))
  ]))
  
  // 8. 记录应用日志
  let access_log = LogRecord::new_with_context(
    Info,
    Some("处理用户请求完成：GET /api/users - 200 OK"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("web_trace_123"),
    Some("web_span_456"),
    None
  )
  Logger::emit(logger, access_log)
  
  // 9. 完成请求处理
  Span::set_status(request_span, Ok, Some("请求处理成功"))
  Span::end(request_span)
}

test "微服务调用链测试" {
  // 模拟微服务间的调用链
  
  // 服务A：API Gateway
  let gateway_tracer = TracerProvider::get_tracer(TracerProvider::default(), "api-gateway")
  let gateway_span = Span::new("gateway.process_request", Server, SpanContext::new("micro_trace_001", "gateway_span_001", true, ""))
  
  // 设置传播上下文
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "x-trace-id", "micro_trace_001")
  TextMapCarrier::set(carrier, "x-span-id", "gateway_span_001")
  TextMapCarrier::set(carrier, "x-request-id", "req_中文_12345")
  
  // 调用服务B：用户服务
  let user_service_span = Span::new("user_service.get_user_info", Client, SpanContext::new("micro_trace_001", "user_span_001", true, ""))
  Span::add_event(user_service_span, "调用用户服务", Some([
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.method", StringValue("GET /users/{id}")),
    ("user.id", StringValue("user_中文_789"))
  ]))
  
  // 服务B内部数据库操作
  let user_db_span = Span::new("user_db.get_user", Client, SpanContext::new("micro_trace_001", "user_db_001", true, ""))
  Span::add_event(user_db_span, "查询用户信息", Some([
    ("db.table", StringValue("users")),
    ("db.operation", StringValue("SELECT")),
    ("db.query_time_ms", IntValue(15)
  ]))
  Span::end(user_db_span)
  Span::end(user_service_span)
  
  // 调用服务C：订单服务
  let order_service_span = Span::new("order_service.get_orders", Client, SpanContext::new("micro_trace_001", "order_span_001", true, ""))
  Span::add_event(order_service_span, "调用订单服务", Some([
    ("service.name", StringValue("order-service")),
    ("service.version", StringValue("1.5.2")),
    ("service.method", StringValue("GET /orders?userId={id}")),
    ("order.status", StringValue("active"))
  ]))
  
  // 服务C内部缓存操作
  let order_cache_span = Span::new("order_cache.get_orders", Client, SpanContext::new("micro_trace_001", "order_cache_001", true, ""))
  Span::add_event(order_cache_span, "缓存查询订单", Some([
    ("cache.key", StringValue("orders:user_中文_789:active")),
    ("cache.hit", BoolValue(true)),
    ("cache.expiry", IntValue(600))
  ]))
  Span::end(order_cache_span)
  Span::end(order_service_span)
  
  // API Gateway聚合响应
  Span::add_event(gateway_span, "聚合服务响应", Some([
    ("services.called", IntValue(2)),
    ("total_response_time_ms", IntValue(85)),
    ("response.size_bytes", IntValue(4096))
  ]))
  
  Span::set_status(gateway_span, Ok, Some("微服务调用成功"))
  Span::end(gateway_span)
}

test "电商系统复杂业务流程测试" {
  // 模拟电商系统的完整业务流程
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "ecommerce-metrics")
  
  // 业务指标
  let order_counter = Meter::create_counter(meter, "orders_total", Some("订单总数"), Some("orders"))
  let revenue_counter = Meter::create_counter(meter, "revenue_total", Some("总收入"), Some("currency"))
  let cart_histogram = Meter::create_histogram(meter, "cart_processing_time_ms", Some("购物车处理时间"), Some("ms"))
  
  // 1. 用户浏览商品
  let browse_span = Span::new("user.browse_products", Internal, SpanContext::new("ecommerce_trace_001", "browse_001", true, ""))
  Span::add_event(browse_span, "用户浏览商品", Some([
    ("user.id", StringValue("customer_中文_001")),
    ("page.category", StringValue("electronics")),
    ("page.products_shown", IntValue(20)),
    ("user.session", StringValue("sess_abc_123"))
  ]))
  
  // 2. 添加商品到购物车
  let cart_span = Span::new("cart.add_item", Internal, SpanContext::new("ecommerce_trace_001", "cart_001", true, ""))
  Span::add_event(cart_span, "添加商品到购物车", Some([
    ("product.id", StringValue("prod_电子_001")),
    ("product.name", StringValue("智能手机 Pro Max")),
    ("product.price", FloatValue(5999.99)),
    ("quantity", IntValue(1)),
    ("cart.total_items", IntValue(3))
  ]))
  
  Histogram::record(cart_histogram, 120.5, None)
  Span::end(cart_span)
  
  // 3. 应用优惠券
  let coupon_span = Span::new("cart.apply_coupon", Internal, SpanContext::new("ecommerce_trace_001", "coupon_001", true, ""))
  Span::add_event(coupon_span, "应用优惠券", Some([
    ("coupon.code", StringValue("SAVE20_中文")),
    ("coupon.discount", FloatValue(200.0)),
    ("coupon.type", StringValue("percentage"))
  ]))
  Span::end(coupon_span)
  
  // 4. 创建订单
  let order_span = Span::new("order.create", Internal, SpanContext::new("ecommerce_trace_001", "order_001", true, ""))
  Span::add_event(order_span, "创建订单", Some([
    ("order.id", StringValue("order_中文_20241228_001")),
    ("order.total_amount", FloatValue(5799.99)),
    ("order.currency", StringValue("CNY")),
    ("payment.method", StringValue("alipay")),
    ("shipping.address", StringValue("北京市朝阳区xxx街道"))
  ]))
  
  // 记录业务指标
  Counter::add(order_counter, 1.0, Some(Attributes::new()))
  Counter::add(revenue_counter, 5799.99, Some(Attributes::new()))
  
  // 5. 支付处理
  let payment_span = Span::new("payment.process", Client, SpanContext::new("ecommerce_trace_001", "payment_001", true, ""))
  Span::add_event(payment_span, "处理支付", Some([
    ("payment.gateway", StringValue("alipay")),
    ("payment.amount", FloatValue(5799.99)),
    ("payment.status", StringValue("processing"))
  ]))
  
  // 支付成功
  Span::add_event(payment_span, "支付成功", Some([
    ("payment.transaction_id", StringValue("txn_alipay_中文_123456")),
    ("payment.timestamp", IntValue(1735689600))
  ]))
  
  Span::set_status(payment_span, Ok, Some("支付处理成功"))
  Span::end(payment_span)
  
  // 6. 库存更新
  let inventory_span = Span::new("inventory.update", Client, SpanContext::new("ecommerce_trace_001", "inventory_001", true, ""))
  Span::add_event(inventory_span, "更新库存", Some([
    ("product.id", StringValue("prod_电子_001")),
    ("warehouse.id", StringValue("warehouse_北京_01")),
    ("old_quantity", IntValue(100)),
    ("new_quantity", IntValue(99))
  ]))
  Span::end(inventory_span)
  
  // 7. 发货处理
  let shipping_span = Span::new("shipping.create", Client, SpanContext::new("ecommerce_trace_001", "shipping_001", true, ""))
  Span::add_event(shipping_span, "创建发货单", Some([
    ("shipping.id", StringValue("ship_中文_001")),
    ("shipping.carrier", StringValue("顺丰快递")),
    ("shipping.tracking_number", StringValue("SF1234567890_中文")),
    ("estimated_delivery", StringValue("2024-12-30"))
  ]))
  Span::end(shipping_span)
  
  Span::set_status(order_span, Ok, Some("订单创建成功"))
  Span::end(order_span)
  Span::end(browse_span)
}

test "IoT设备数据处理流程测试" {
  // 模拟IoT设备数据收集和处理流程
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "iot-processor")
  
  // 1. 设备数据接收
  let device_span = Span::new("iot.data_ingestion", Server, SpanContext::new("iot_trace_001", "ingestion_001", true, ""))
  Span::add_event(device_span, "接收设备数据", Some([
    ("device.id", StringValue("sensor_温度_001")),
    ("device.type", StringValue("temperature_sensor")),
    ("device.location", StringValue("工厂车间A_生产线1")),
    ("device.firmware", StringValue("v2.1.3_中文版"))
  ]))
  
  // 2. 数据验证
  let validation_span = Span::new("data.validation", Internal, SpanContext::new("iot_trace_001", "validation_001", true, ""))
  Span::add_event(validation_span, "验证数据格式", Some([
    ("data.format", StringValue("json")),
    ("data.schema_version", StringValue("1.0")),
    ("validation.rules", IntValue(5)),
    ("validation.result", StringValue("passed"))
  ]))
  Span::end(validation_span)
  
  // 3. 数据处理和转换
  let processing_span = Span::new("data.processing", Internal, SpanContext::new("iot_trace_001", "processing_001", true, ""))
  Span::add_event(processing_span, "数据处理", Some([
    ("processing.algorithm", StringValue("temperature_normalization")),
    ("input.range.min", FloatValue(-40.0)),
    ("input.range.max", FloatValue(125.0)),
    ("output.range.min", FloatValue(0.0)),
    ("output.range.max", FloatValue(100.0))
  ]))
  
  // 模拟处理多个数据点
  for i = 0; i < 100; i = i + 1 {
    let data_point = LogRecord::new_with_context(
      Info,
      Some("处理数据点 ${i}"),
      Some(Attributes::new()),
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some("iot_trace_001"),
      Some("processing_001"),
      None
    )
    Logger::emit(logger, data_point)
  }
  
  Span::end(processing_span)
  
  // 4. 异常检测
  let anomaly_span = Span::new("anomaly.detection", Internal, SpanContext::new("iot_trace_001", "anomaly_001", true, ""))
  Span::add_event(anomaly_span, "异常检测", Some([
    ("detection.algorithm", StringValue("statistical_outlier")),
    ("threshold.value", FloatValue(95.0)),
    ("anomalies.detected", IntValue(2)),
    ("anomaly.severity", StringValue("medium"))
  ]))
  
  // 记录异常日志
  let anomaly_log = LogRecord::new_with_context(
    Warn,
    Some("检测到温度异常：设备 sensor_温度_001 在时间点 ${timestamp} 检测到异常温度值"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("iot_trace_001"),
    Some("anomaly_001"),
    None
  )
  Logger::emit(logger, anomaly_log)
  
  Span::end(anomaly_span)
  
  // 5. 数据存储
  let storage_span = Span::new("data.storage", Client, SpanContext::new("iot_trace_001", "storage_001", true, ""))
  Span::add_event(storage_span, "存储处理后的数据", Some([
    ("storage.system", StringValue("timescaledb")),
    ("table.name", StringValue("sensor_readings")),
    ("batch.size", IntValue(100),
    ("compression.ratio", FloatValue(0.65))
  ]))
  Span::end(storage_span)
  
  // 6. 实时告警
  let alert_span = Span::new("alert.notification", Client, SpanContext::new("iot_trace_001", "alert_001", true, ""))
  Span::add_event(alert_span, "发送告警通知", Some([
    ("alert.type", StringValue("temperature_anomaly")),
    ("alert.priority", StringValue("medium")),
    ("notification.channels", ArrayStringValue(["email", "sms", "webhook"])),
    ("recipients", ArrayStringValue(["operator_中文_001", "manager_技术_002"]))
  ]))
  Span::end(alert_span)
  
  Span::set_status(device_span, Ok, Some("IoT数据处理完成"))
  Span::end(device_span)
}

test "金融交易处理系统测试" {
  // 模拟金融交易处理系统的遥测场景
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "financial-service")
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "financial-metrics")
  
  // 金融业务指标
  let transaction_counter = Meter::create_counter(meter, "transactions_total", Some("交易总数"), Some("transactions"))
  let amount_histogram = Meter::create_histogram(meter, "transaction_amount", Some("交易金额分布"), Some("currency"))
  let latency_histogram = Meter::create_histogram(meter, "transaction_latency_ms", Some("交易延迟"), Some("ms"))
  
  // 1. 交易请求接收
  let trade_span = Span::new("trade.execute", Server, SpanContext::new("finance_trace_001", "trade_001", true, ""))
  Span::add_event(trade_span, "接收交易请求", Some([
    ("transaction.id", StringValue("txn_证券_20241228_001")),
    ("account.id", StringValue("acc_中文_wealth_001")),
    ("security.symbol", StringValue("000001.SZ")),
    ("order.type", StringValue("BUY")),
    ("order.quantity", IntValue(1000)),
    ("order.price", FloatValue(15.68))
  ]))
  
  // 2. 风险检查
  let risk_span = Span::new("risk.check", Internal, SpanContext::new("finance_trace_001", "risk_001", true, ""))
  Span::add_event(risk_span, "执行风险检查", Some([
    ("risk.engine", StringValue("realtime_risk_v3")),
    ("check.types", ArrayStringValue(["position_limit", "credit_limit", "market_risk"])),
    ("risk.score", FloatValue(0.35)),
    ("risk.level", StringValue("LOW"))
  ]))
  Span::end(risk_span)
  
  // 3. 账户余额验证
  let balance_span = Span::new("balance.validate", Client, SpanContext::new("finance_trace_001", "balance_001", true, ""))
  Span::add_event(balance_span, "验证账户余额", Some([
    ("account.balance", FloatValue(50000.0)),
    ("required.amount", FloatValue(15680.0)),
    ("currency", StringValue("CNY")),
    ("frozen.amount", FloatValue(0.0))
  ]))
  Span::end(balance_span)
  
  // 4. 订单路由到交易所
  let routing_span = Span::new("order.route", Client, SpanContext::new("finance_trace_001", "routing_001", true, ""))
  Span::add_event(routing_span, "路由订单到交易所", Some([
    ("exchange.name", StringValue("深圳证券交易所")),
    ("gateway.id", StringValue("gateway_证券_001")),
    ("routing.algorithm", StringValue("best_execution")),
    ("estimated.latency_ms", IntValue(25)
  ]))
  Span::end(routing_span)
  
  // 5. 交易所执行
  let exchange_span = Span::new("exchange.execute", Client, SpanContext::new("finance_trace_001", "exchange_001", true, ""))
  Span::add_event(exchange_span, "交易所执行订单", Some([
    ("exchange.order_id", StringValue("exch_20241228_001_中文")),
    ("execution.price", FloatValue(15.67)),
    ("executed.quantity", IntValue(1000)),
    ("execution.time", StringValue("2024-12-28T09:30:15.123Z"))
  ]))
  Span::end(exchange_span)
  
  // 6. 清算和结算
  let settlement_span = Span::new("settlement.process", Client, SpanContext::new("finance_trace_001", "settlement_001", true, ""))
  Span::add_event(settlement_span, "处理清算结算", Some([
    ("settlement.system", StringValue("central_clearing_house")),
    ("settlement.date", StringValue("T+1")),
    ("settlement.amount", FloatValue(15670.0)),
    ("commission.fee", FloatValue(15.67),
    ("tax.amount", FloatValue(0.0))
  ]))
  Span::end(settlement_span)
  
  // 7. 更新账户持仓
  let position_span = Span::new("position.update", Client, SpanContext::new("finance_trace_001", "position_001", true, ""))
  Span::add_event(position_span, "更新账户持仓", Some([
    ("security.symbol", StringValue("000001.SZ")),
    ("old.quantity", IntValue(0)),
    ("new.quantity", IntValue(1000)),
    ("average.price", FloatValue(15.67)),
    ("market.value", FloatValue(15670.0))
  ]))
  Span::end(position_span)
  
  // 记录交易指标
  Counter::add(transaction_counter, 1.0, Some(Attributes::new()))
  Histogram::record(amount_histogram, 15670.0, None)
  Histogram::record(latency_histogram, 125.5, None)
  
  Span::set_status(trade_span, Ok, Some("交易执行成功"))
  Span::end(trade_span)
}