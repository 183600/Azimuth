// Azimuth Telemetry System - 复杂场景集成测试用例
// 测试系统在复杂真实场景中的综合表现

test "电商订单处理流程集成测试" {
  // 模拟完整的电商订单处理流程
  
  // 1. 用户服务 - 用户登录
  let user_tracer_provider = TracerProvider::default()
  let user_tracer = TracerProvider::get_tracer(user_tracer_provider, "user-service")
  let login_span = Tracer::start_span(user_tracer, "user-login")
  Span::set_attribute(login_span, "user.id", "user-12345")
  Span::set_attribute(login_span, "login.method", "password")
  
  // 记录用户登录指标
  let user_meter_provider = MeterProvider::default()
  let user_meter = MeterProvider::get_meter(user_meter_provider, "user-service")
  let login_counter = Meter::create_counter(user_meter, "user_logins_total", Some("Total user logins"), Some("count"))
  Counter::add(login_counter, 1.0)
  
  // 记录登录日志
  let user_logger_provider = LoggerProvider::default()
  let user_logger = LoggerProvider::get_logger(user_logger_provider, "user-service")
  let login_log = LogRecord::new_with_context(
    Info,
    Some("User logged in successfully"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(login_span))),
    Some(SpanContext::span_id(Span::span_context(login_span))),
    Some(Context::root())
  )
  Logger::emit(user_logger, login_log)
  
  // 2. 商品服务 - 浏览商品
  let product_tracer_provider = TracerProvider::default()
  let product_tracer = TracerProvider::get_tracer(product_tracer_provider, "product-service")
  let browse_span = Tracer::start_span(product_tracer, "browse-products")
  Span::set_attribute(browse_span, "category", "electronics")
  Span::set_attribute(browse_span, "page.size", "20")
  
  // 记录商品浏览指标
  let product_meter = MeterProvider::get_meter(user_meter_provider, "product-service")
  let browse_counter = Meter::create_counter(product_meter, "product_browses_total", Some("Total product browses"), Some("count"))
  Counter::add(browse_counter, 1.0)
  
  // 3. 购物车服务 - 添加商品到购物车
  let cart_tracer_provider = TracerProvider::default()
  let cart_tracer = TracerProvider::get_tracer(cart_tracer_provider, "cart-service")
  let add_to_cart_span = Tracer::start_span(cart_tracer, "add-to-cart")
  Span::set_attribute(add_to_cart_span, "product.id", "prod-67890")
  Span::set_attribute(add_to_cart_span, "quantity", "2")
  Span::set_attribute(add_to_cart_span, "price", "299.99")
  
  // 记录购物车指标
  let cart_meter = MeterProvider::get_meter(user_meter_provider, "cart-service")
  let cart_counter = Meter::create_counter(cart_meter, "items_added_to_cart_total", Some("Total items added to cart"), Some("count"))
  Counter::add(cart_counter, 2.0)
  
  // 4. 订单服务 - 创建订单
  let order_tracer_provider = TracerProvider::default()
  let order_tracer = TracerProvider::get_tracer(order_tracer_provider, "order-service")
  let create_order_span = Tracer::start_span(order_tracer, "create-order")
  Span::set_attribute(create_order_span, "order.id", "order-11111")
  Span::set_attribute(create_order_span, "total.amount", "599.98")
  Span::set_attribute(create_order_span, "payment.method", "credit_card")
  
  // 记录订单指标
  let order_meter = MeterProvider::get_meter(user_meter_provider, "order-service")
  let order_counter = Meter::create_counter(order_meter, "orders_created_total", Some("Total orders created"), Some("count"))
  let order_amount_histogram = Meter::create_histogram(order_meter, "order_amount", Some("Order amount"), Some("usd"))
  Counter::add(order_counter, 1.0)
  Histogram::record(order_amount_histogram, 599.98)
  
  // 5. 支付服务 - 处理支付
  let payment_tracer_provider = TracerProvider::default()
  let payment_tracer = TracerProvider::get_tracer(payment_tracer_provider, "payment-service")
  let process_payment_span = Tracer::start_span(payment_tracer, "process-payment")
  Span::set_attribute(process_payment_span, "payment.id", "pay-22222")
  Span::set_attribute(process_payment_span, "payment.status", "success")
  Span::set_attribute(process_payment_span, "processing.time", "1500")
  
  // 记录支付指标
  let payment_meter = MeterProvider::get_meter(user_meter_provider, "payment-service")
  let payment_counter = Meter::create_counter(payment_meter, "payments_processed_total", Some("Total payments processed"), Some("count"))
  let payment_duration_histogram = Meter::create_histogram(payment_meter, "payment_processing_duration", Some("Payment processing duration"), Some("ms"))
  Counter::add(payment_counter, 1.0)
  Histogram::record(payment_duration_histogram, 1500.0)
  
  // 6. 物流服务 - 安排发货
  let shipping_tracer_provider = TracerProvider::default()
  let shipping_tracer = TracerProvider::get_tracer(shipping_tracer_provider, "shipping-service")
  let arrange_shipping_span = Tracer::start_span(shipping_tracer, "arrange-shipping")
  Span::set_attribute(arrange_shipping_span, "shipping.id", "ship-33333")
  Span::set_attribute(arrange_shipping_span, "carrier", "fedex")
  Span::set_attribute(arrange_shipping_span, "estimated.delivery", "2025-01-05")
  
  // 记录物流指标
  let shipping_meter = MeterProvider::get_meter(user_meter_provider, "shipping-service")
  let shipping_counter = Meter::create_counter(shipping_meter, "shipments_arranged_total", Some("Total shipments arranged"), Some("count"))
  Counter::add(shipping_counter, 1.0)
  
  // 7. 通知服务 - 发送确认邮件
  let notification_tracer_provider = TracerProvider::default()
  let notification_tracer = TracerProvider::get_tracer(notification_tracer_provider, "notification-service")
  let send_email_span = Tracer::start_span(notification_tracer, "send-order-confirmation")
  Span::set_attribute(send_email_span, "notification.id", "notif-44444")
  Span::set_attribute(send_email_span, "notification.type", "email")
  Span::set_attribute(send_email_span, "recipient", "user@example.com")
  
  // 记录通知指标
  let notification_meter = MeterProvider::get_meter(user_meter_provider, "notification-service")
  let notification_counter = Meter::create_counter(notification_meter, "notifications_sent_total", Some("Total notifications sent"), Some("count"))
  Counter::add(notification_counter, 1.0)
  
  // 验证所有服务和组件正常工作
  assert_eq(Span::name(login_span), "user-login")
  assert_eq(Span::name(browse_span), "browse-products")
  assert_eq(Span::name(add_to_cart_span), "add-to-cart")
  assert_eq(Span::name(create_order_span), "create-order")
  assert_eq(Span::name(process_payment_span), "process-payment")
  assert_eq(Span::name(arrange_shipping_span), "arrange-shipping")
  assert_eq(Span::name(send_email_span), "send-order-confirmation")
  
  // 结束所有Span
  Span::end(send_email_span)
  Span::end(arrange_shipping_span)
  Span::end(process_payment_span)
  Span::end(create_order_span)
  Span::end(add_to_cart_span)
  Span::end(browse_span)
  Span::end(login_span)
}

test "微服务错误处理和恢复集成测试" {
  // 模拟微服务中的错误处理和恢复场景
  
  // 1. API Gateway - 接收请求
  let gateway_tracer_provider = TracerProvider::default()
  let gateway_tracer = TracerProvider::get_tracer(gateway_tracer_provider, "api-gateway")
  let gateway_span = Tracer::start_span(gateway_tracer, "gateway-request")
  Span::set_attribute(gateway_span, "request.path", "/api/orders")
  Span::set_attribute(gateway_span, "request.method", "POST")
  Span::set_attribute(gateway_span, "user.id", "user-12345")
  
  // 2. 订单服务 - 处理订单创建（遇到错误）
  let order_tracer_provider = TracerProvider::default()
  let order_tracer = TracerProvider::get_tracer(order_tracer_provider, "order-service")
  let order_span = Tracer::start_span(order_tracer, "create-order")
  Span::set_attribute(order_span, "order.id", "order-error-123")
  
  // 模拟库存不足错误
  Span::add_event(order_span, "inventory_check_failed", Some([
    ("product.id", StringValue("prod-67890")),
    ("requested.quantity", StringValue("5")),
    ("available.quantity", StringValue("2"))
  ]))
  
  Span::set_status(order_span, Error, Some("Insufficient inventory"))
  
  // 记录错误指标
  let order_meter_provider = MeterProvider::default()
  let order_meter = MeterProvider::get_meter(order_meter_provider, "order-service")
  let error_counter = Meter::create_counter(order_meter, "order_errors_total", Some("Total order errors"), Some("count"))
  Counter::add(error_counter, 1.0)
  
  // 记录错误日志
  let order_logger_provider = LoggerProvider::default()
  let order_logger = LoggerProvider::get_logger(order_logger_provider, "order-service")
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Order creation failed due to insufficient inventory"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(order_span))),
    Some(SpanContext::span_id(Span::span_context(order_span))),
    Some(Context::root())
  )
  Logger::emit(order_logger, error_log)
  
  // 3. 重试机制 - 尝试恢复
  let retry_span = Tracer::start_span(order_tracer, "order-retry")
  Span::set_attribute(retry_span, "retry.count", "1")
  Span::set_attribute(retry_span, "retry.strategy", "exponential_backoff")
  
  // 模拟重试仍然失败
  Span::add_event(retry_span, "retry_failed", Some([
    ("error.type", StringValue("inventory_insufficient")),
    ("retry.delay", StringValue("1000"))
  ]))
  
  Span::set_status(retry_span, Error, Some("Retry attempts exhausted"))
  
  // 记录重试指标
  let retry_counter = Meter::create_counter(order_meter, "order_retries_total", Some("Total order retries"), Some("count"))
  Counter::add(retry_counter, 1.0)
  
  // 4. 降级处理 - 提供备选方案
  let fallback_span = Tracer::start_span(order_tracer, "order-fallback")
  Span::set_attribute(fallback_span, "fallback.strategy", "partial_order")
  Span::set_attribute(fallback_span, "adjusted.quantity", "2")
  
  // 降级成功
  Span::set_status(fallback_span, Ok, Some("Order created with adjusted quantity"))
  
  // 记录降级指标
  let fallback_counter = Meter::create_counter(order_meter, "order_fallbacks_total", Some("Total order fallbacks"), Some("count"))
  Counter::add(fallback_counter, 1.0)
  
  // 记录降级日志
  let fallback_log = LogRecord::new_with_context(
    Warn,
    Some("Order created with fallback strategy due to inventory constraints"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(fallback_span))),
    Some(SpanContext::span_id(Span::span_context(fallback_span))),
    Some(Context::root())
  )
  Logger::emit(order_logger, fallback_log)
  
  // 5. 通知服务 - 发送降级通知
  let notification_tracer_provider = TracerProvider::default()
  let notification_tracer = TracerProvider::get_tracer(notification_tracer_provider, "notification-service")
  let notification_span = Tracer::start_span(notification_tracer, "send-fallback-notification")
  Span::set_attribute(notification_span, "notification.type", "email")
  Span::set_attribute(notification_span, "notification.reason", "order_adjustment")
  
  // 验证错误处理流程
  assert_eq(Span::status(order_span), Error)
  assert_eq(Span::status(retry_span), Error)
  assert_eq(Span::status(fallback_span), Ok)
  
  // 结束所有Span
  Span::end(notification_span)
  Span::end(fallback_span)
  Span::end(retry_span)
  Span::end(order_span)
  Span::end(gateway_span)
}

test "实时数据处理管道集成测试" {
  // 模拟实时数据处理管道
  
  // 1. 数据采集服务
  let ingest_tracer_provider = TracerProvider::default()
  let ingest_tracer = TracerProvider::get_tracer(ingest_tracer_provider, "data-ingestion")
  let ingest_span = Tracer::start_span(ingest_tracer, "ingest-events")
  Span::set_attribute(ingest_span, "source", "iot-sensors")
  Span::set_attribute(ingest_span, "batch.size", "1000")
  Span::set_attribute(ingest_span, "data.format", "json")
  
  // 记录采集指标
  let ingest_meter_provider = MeterProvider::default()
  let ingest_meter = MeterProvider::get_meter(ingest_meter_provider, "data-ingestion")
  let events_counter = Meter::create_counter(ingest_meter, "events_ingested_total", Some("Total events ingested"), Some("count"))
  let data_size_histogram = Meter::create_histogram(ingest_meter, "ingested_data_size", Some("Ingested data size"), Some("bytes"))
  Counter::add(events_counter, 1000.0)
  Histogram::record(data_size_histogram, 2048000.0) // 2MB
  
  // 2. 数据验证服务
  let validation_tracer_provider = TracerProvider::default()
  let validation_tracer = TracerProvider::get_tracer(validation_tracer_provider, "data-validation")
  let validation_span = Tracer::start_span(validation_tracer, "validate-events")
  Span::set_attribute(validation_span, "validation.rules", "schema,format,range")
  Span::set_attribute(validation_span, "valid.events", "950")
  Span::set_attribute(validation_span, "invalid.events", "50")
  
  // 记录验证指标
  let validation_meter = MeterProvider::get_meter(ingest_meter_provider, "data-validation")
  let validation_counter = Meter::create_counter(validation_meter, "events_validated_total", Some("Total events validated"), Some("count"))
  let invalid_counter = Meter::create_counter(validation_meter, "invalid_events_total", Some("Total invalid events"), Some("count"))
  Counter::add(validation_counter, 1000.0)
  Counter::add(invalid_counter, 50.0)
  
  // 3. 数据转换服务
  let transform_tracer_provider = TracerProvider::default()
  let transform_tracer = TracerProvider::get_tracer(transform_tracer_provider, "data-transformation")
  let transform_span = Tracer::start_span(transform_tracer, "transform-events")
  Span::set_attribute(transform_span, "transformation.type", "enrichment")
  Span::set_attribute(transform_span, "enrichment.fields", "location,device_type,user_segment")
  
  // 记录转换指标
  let transform_meter = MeterProvider::get_meter(ingest_meter_provider, "data-transformation")
  let transform_counter = Meter::create_counter(transform_meter, "events_transformed_total", Some("Total events transformed"), Some("count"))
  let transform_duration_histogram = Meter::create_histogram(transform_meter, "transformation_duration", Some("Transformation duration"), Some("ms"))
  Counter::add(transform_counter, 950.0)
  Histogram::record(transform_duration_histogram, 250.0)
  
  // 4. 数据聚合服务
  let aggregation_tracer_provider = TracerProvider::default()
  let aggregation_tracer = TracerProvider::get_tracer(aggregation_tracer_provider, "data-aggregation")
  let aggregation_span = Tracer::start_span(aggregation_tracer, "aggregate-events")
  Span::set_attribute(aggregation_span, "aggregation.window", "1m")
  Span::set_attribute(aggregation_span, "aggregation.keys", "user_id,device_type,location")
  Span::set_attribute(aggregation_span, "aggregated.metrics", "count,sum,avg,max,min")
  
  // 记录聚合指标
  let aggregation_meter = MeterProvider::get_meter(ingest_meter_provider, "data-aggregation")
  let aggregation_counter = Meter::create_counter(aggregation_meter, "events_aggregated_total", Some("Total events aggregated"), Some("count"))
  Counter::add(aggregation_counter, 950.0)
  
  // 5. 数据存储服务
  let storage_tracer_provider = TracerProvider::default()
  let storage_tracer = TracerProvider::get_tracer(storage_tracer_provider, "data-storage")
  let storage_span = Tracer::start_span(storage_tracer, "store-aggregated-data")
  Span::set_attribute(storage_span, "storage.type", "timeseries_db")
  Span::set_attribute(storage_span, "table.name", "aggregated_metrics")
  Span::set_attribute(storage_span, "records.stored", "50")
  
  // 记录存储指标
  let storage_meter = MeterProvider::get_meter(ingest_meter_provider, "data-storage")
  let storage_counter = Meter::create_counter(storage_meter, "records_stored_total", Some("Total records stored"), Some("count"))
  let storage_duration_histogram = Meter::create_histogram(storage_meter, "storage_duration", Some("Storage duration"), Some("ms"))
  Counter::add(storage_counter, 50.0)
  Histogram::record(storage_duration_histogram, 120.0)
  
  // 6. 实时通知服务
  let alert_tracer_provider = TracerProvider::default()
  let alert_tracer = TracerProvider::get_tracer(alert_tracer_provider, "alert-service")
  let alert_span = Tracer::start_span(alert_tracer, "check-alerts")
  Span::set_attribute(alert_span, "alert.rules.checked", "10")
  Span::set_attribute(alert_span, "alerts.triggered", "2")
  
  // 记录告警指标
  let alert_meter = MeterProvider::get_meter(ingest_meter_provider, "alert-service")
  let alert_counter = Meter::create_counter(alert_meter, "alerts_triggered_total", Some("Total alerts triggered"), Some("count"))
  Counter::add(alert_counter, 2.0)
  
  // 验证数据处理管道
  assert_eq(Span::name(ingest_span), "ingest-events")
  assert_eq(Span::name(validation_span), "validate-events")
  assert_eq(Span::name(transform_span), "transform-events")
  assert_eq(Span::name(aggregation_span), "aggregate-events")
  assert_eq(Span::name(storage_span), "store-aggregated-data")
  assert_eq(Span::name(alert_span), "check-alerts")
  
  // 结束所有Span
  Span::end(alert_span)
  Span::end(storage_span)
  Span::end(aggregation_span)
  Span::end(transform_span)
  Span::end(validation_span)
  Span::end(ingest_span)
}

test "多租户SaaS平台集成测试" {
  // 模拟多租户SaaS平台的遥测数据收集
  
  // 租户A的操作
  let tenant_a_tracer_provider = TracerProvider::default()
  let tenant_a_tracer = TracerProvider::get_tracer(tenant_a_tracer_provider, "tenant-a-service")
  let tenant_a_span = Tracer::start_span(tenant_a_tracer, "tenant-operation")
  Span::set_attribute(tenant_a_span, "tenant.id", "tenant-abc-123")
  Span::set_attribute(tenant_a_span, "tenant.plan", "enterprise")
  Span::set_attribute(tenant_a_span, "operation.type", "data-export")
  
  // 租户A的指标
  let tenant_a_meter_provider = MeterProvider::default()
  let tenant_a_meter = MeterProvider::get_meter(tenant_a_meter_provider, "tenant-a-service")
  let tenant_a_counter = Meter::create_counter(tenant_a_meter, "tenant_operations_total", Some("Total tenant operations"), Some("count"))
  Counter::add(tenant_a_counter, 1.0)
  
  // 租户B的操作
  let tenant_b_tracer_provider = TracerProvider::default()
  let tenant_b_tracer = TracerProvider::get_tracer(tenant_b_tracer_provider, "tenant-b-service")
  let tenant_b_span = Tracer::start_span(tenant_b_tracer, "tenant-operation")
  Span::set_attribute(tenant_b_span, "tenant.id", "tenant-xyz-789")
  Span::set_attribute(tenant_b_span, "tenant.plan", "starter")
  Span::set_attribute(tenant_b_span, "operation.type", "report-generation")
  
  // 租户B的指标
  let tenant_b_meter_provider = MeterProvider::default()
  let tenant_b_meter = MeterProvider::get_meter(tenant_b_meter_provider, "tenant-b-service")
  let tenant_b_counter = Meter::create_counter(tenant_b_meter, "tenant_operations_total", Some("Total tenant operations"), Some("count"))
  Counter::add(tenant_b_counter, 1.0)
  
  // 平台级别的聚合
  let platform_tracer_provider = TracerProvider::default()
  let platform_tracer = TracerProvider::get_tracer(platform_tracer_provider, "platform-service")
  let platform_span = Tracer::start_span(platform_tracer, "platform-metrics-aggregation")
  Span::set_attribute(platform_span, "total.tenants", "2")
  Span::set_attribute(platform_span, "total.operations", "2")
  Span::set_attribute(platform_span, "aggregation.period", "1m")
  
  // 平台指标
  let platform_meter_provider = MeterProvider::default()
  let platform_meter = MeterProvider::get_meter(platform_meter_provider, "platform-service")
  let platform_counter = Meter::create_counter(platform_meter, "platform_operations_total", Some("Total platform operations"), Some("count"))
  let tenant_histogram = Meter::create_histogram(platform_meter, "tenant_operation_duration", Some("Tenant operation duration"), Some("ms"))
  Counter::add(platform_counter, 2.0)
  Histogram::record(tenant_histogram, 500.0) // 租户A操作耗时
  Histogram::record(tenant_histogram, 300.0) // 租户B操作耗时
  
  // 验证多租户隔离
  assert_eq(Span::get_attribute(tenant_a_span, "tenant.id"), Some(StringValue("tenant-abc-123")))
  assert_eq(Span::get_attribute(tenant_b_span, "tenant.id"), Some(StringValue("tenant-xyz-789")))
  
  // 结束所有Span
  Span::end(platform_span)
  Span::end(tenant_b_span)
  Span::end(tenant_a_span)
}

test "混合云环境集成测试" {
  // 模拟混合云环境中的遥测数据收集
  
  // 本地数据中心服务
  let onprem_tracer_provider = TracerProvider::default()
  let onprem_tracer = TracerProvider::get_tracer(onprem_tracer_provider, "onprem-service")
  let onprem_span = Tracer::start_span(onprem_tracer, "onprem-operation")
  Span::set_attribute(onprem_span, "deployment.location", "onprem")
  Span::set_attribute(onprem_span, "datacenter", "dc-west-1")
  Span::set_attribute(onprem_span, "operation.type", "database-query")
  
  // 本地指标
  let onprem_meter_provider = MeterProvider::default()
  let onprem_meter = MeterProvider::get_meter(onprem_meter_provider, "onprem-service")
  let onprem_counter = Meter::create_counter(onprem_meter, "onprem_operations_total", Some("Total onprem operations"), Some("count"))
  Counter::add(onprem_counter, 1.0)
  
  // 云服务A - AWS
  let aws_tracer_provider = TracerProvider::default()
  let aws_tracer = TracerProvider::get_tracer(aws_tracer_provider, "aws-service")
  let aws_span = Tracer::start_span(aws_tracer, "cloud-operation")
  Span::set_attribute(aws_span, "deployment.location", "cloud")
  Span::set_attribute(aws_span, "cloud.provider", "aws")
  Span::set_attribute(aws_span, "region", "us-west-2")
  Span::set_attribute(aws_span, "operation.type", "api-call")
  
  // AWS指标
  let aws_meter_provider = MeterProvider::default()
  let aws_meter = MeterProvider::get_meter(aws_meter_provider, "aws-service")
  let aws_counter = Meter::create_counter(aws_meter, "aws_operations_total", Some("Total AWS operations"), Some("count"))
  Counter::add(aws_counter, 1.0)
  
  // 云服务B - Azure
  let azure_tracer_provider = TracerProvider::default()
  let azure_tracer = TracerProvider::get_tracer(azure_tracer_provider, "azure-service")
  let azure_span = Tracer::start_span(azure_tracer, "cloud-operation")
  Span::set_attribute(azure_span, "deployment.location", "cloud")
  Span::set_attribute(azure_span, "cloud.provider", "azure")
  Span::set_attribute(azure_span, "region", "eastus")
  Span::set_attribute(azure_span, "operation.type", "storage-operation")
  
  // Azure指标
  let azure_meter_provider = MeterProvider::default()
  let azure_meter = MeterProvider::get_meter(azure_meter_provider, "azure-service")
  let azure_counter = Meter::create_counter(azure_meter, "azure_operations_total", Some("Total Azure operations"), Some("count"))
  Counter::add(azure_counter, 1.0)
  
  // 混合云管理平台
  let hybrid_tracer_provider = TracerProvider::default()
  let hybrid_tracer = TracerProvider::get_tracer(hybrid_tracer_provider, "hybrid-platform")
  let hybrid_span = Tracer::start_span(hybrid_tracer, "hybrid-orchestration")
  Span::set_attribute(hybrid_span, "total.environments", "3")
  Span::set_attribute(hybrid_span, "onprem.services", "1")
  Span::set_attribute(hybrid_span, "cloud.services", "2")
  Span::set_attribute(hybrid_span, "cloud.providers", "aws,azure")
  
  // 混合云指标
  let hybrid_meter_provider = MeterProvider::default()
  let hybrid_meter = MeterProvider::get_meter(hybrid_meter_provider, "hybrid-platform")
  let hybrid_counter = Meter::create_counter(hybrid_meter, "hybrid_operations_total", Some("Total hybrid operations"), Some("count"))
  let cross_cloud_histogram = Meter::create_histogram(hybrid_meter, "cross_cloud_latency", Some("Cross-cloud latency"), Some("ms"))
  Counter::add(hybrid_counter, 3.0)
  Histogram::record(cross_cloud_histogram, 25.0)  // 本地到AWS延迟
  Histogram::record(cross_cloud_histogram, 35.0)  // 本地到Azure延迟
  Histogram::record(cross_cloud_histogram, 45.0)  // AWS到Azure延迟
  
  // 验证混合云环境
  assert_eq(Span::get_attribute(onprem_span, "deployment.location"), Some(StringValue("onprem")))
  assert_eq(Span::get_attribute(aws_span, "deployment.location"), Some(StringValue("cloud")))
  assert_eq(Span::get_attribute(azure_span, "deployment.location"), Some(StringValue("cloud")))
  
  // 结束所有Span
  Span::end(hybrid_span)
  Span::end(azure_span)
  Span::end(aws_span)
  Span::end(onprem_span)
}