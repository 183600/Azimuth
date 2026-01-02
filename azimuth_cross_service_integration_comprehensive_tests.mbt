// Azimuth跨服务集成测试用例
// 测试多个服务之间的遥测数据传递和一致性

test "跨服务上下文传播" {
  // 创建服务A的遥测上下文
  let service_a_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-a")
  let service_a_span = Tracer::start_span(service_a_tracer, "service-a.operation")
  
  // 设置服务A的span属性
  Span::set_attribute(service_a_span, "service.name", "service-a")
  Span::set_attribute(service_a_span, "service.version", "1.2.3")
  Span::set_attribute(service_a_span, "operation.type", "user-authentication")
  
  // 获取服务A的span上下文
  let service_a_context = Span::span_context(service_a_span)
  assert_true(SpanContext::is_valid(service_a_context))
  
  // 模拟跨服务调用：将上下文注入到HTTP头
  let headers = []
  let injected_headers = ContextPropagator::inject_to_headers(service_a_context, headers)
  
  // 验证注入的头部包含必要的上下文信息
  let trace_id_header = injected_headers.find(fn(h) { h.0 == "traceparent" })
  assert_true(trace_id_header.is_some())
  
  // 服务B接收请求并提取上下文
  let service_b_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-b")
  let extracted_context = ContextPropagator::extract_from_headers(injected_headers)
  
  // 创建服务B的子span
  let service_b_span = Tracer::start_span_with_parent(
    service_b_tracer, 
    "service-b.operation", 
    extracted_context
  )
  
  // 设置服务B的span属性
  Span::set_attribute(service_b_span, "service.name", "service-b")
  Span::set_attribute(service_b_span, "service.version", "2.1.0")
  Span::set_attribute(service_b_span, "operation.type", "token-validation")
  
  // 验证服务B的span与服务A的span属于同一个trace
  let service_b_context = Span::span_context(service_b_span)
  assert_eq(SpanContext::trace_id(service_a_context), SpanContext::trace_id(service_b_context))
  assert_eq(SpanContext::span_id(service_a_context), SpanContext::parent_span_id(service_b_context))
  
  // 结束所有span
  Span::end(service_b_span)
  Span::end(service_a_span)
  
  assert_true(true)
}

test "跨服务度量数据一致性" {
  // 创建服务A和服务B的度量提供者
  let service_a_meter = MeterProvider::get_meter(MeterProvider::default(), "service-a")
  let service_b_meter = MeterProvider::get_meter(MeterProvider::default(), "service-b")
  
  // 创建服务A的计数器
  let service_a_requests = Meter::create_counter(
    service_a_meter, 
    "http.requests.total", 
    Some("Total HTTP requests"), 
    Some("count")
  )
  
  // 创建服务B的计数器（相同名称和描述）
  let service_b_requests = Meter::create_counter(
    service_b_meter, 
    "http.requests.total", 
    Some("Total HTTP requests"), 
    Some("count")
  )
  
  // 服务A记录请求
  Counter::add_with_attributes(service_a_requests, 1.0, [
    ("method", "GET"),
    ("endpoint", "/api/auth"),
    ("status", "200"),
    ("service", "service-a")
  ])
  
  // 服务B记录请求（作为服务A调用的结果）
  Counter::add_with_attributes(service_b_requests, 1.0, [
    ("method", "POST"),
    ("endpoint", "/api/validate"),
    ("status", "200"),
    ("service", "service-b")
  ])
  
  // 创建跨服务关联的度量
  let cross_service_meter = MeterProvider::get_meter(MeterProvider::default(), "cross-service")
  let cross_service_latency = Meter::create_histogram(
    cross_service_meter,
    "cross.service.latency",
    Some("Cross-service request latency"),
    Some("milliseconds")
  )
  
  // 记录跨服务延迟
  Histogram::record_with_attributes(cross_service_latency, 125.5, [
    ("source.service", "service-a"),
    ("target.service", "service-b"),
    ("operation", "user-authentication")
  ])
  
  // 验证度量数据的一致性
  assert_eq(service_a_requests.name, "http.requests.total")
  assert_eq(service_b_requests.name, "http.requests.total")
  assert_eq(service_a_requests.description, Some("Total HTTP requests"))
  assert_eq(service_b_requests.description, Some("Total HTTP requests"))
  assert_eq(service_a_requests.unit, Some("count"))
  assert_eq(service_b_requests.unit, Some("count"))
  
  assert_true(true)
}

test "跨服务日志关联性" {
  // 创建服务A的日志记录器
  let service_a_logger = LoggerProvider::get_logger(LoggerProvider::default(), "service-a")
  
  // 创建服务A的span
  let service_a_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-a")
  let service_a_span = Tracer::start_span(service_a_tracer, "service-a.processing")
  
  // 服务A记录日志（与span关联）
  let service_a_log = LogRecord::new(Info, "Processing user authentication request")
  LogRecord::add_attribute(service_a_log, "user.id", "user-12345")
  LogRecord::add_attribute(service_a_log, "request.id", "req-abc-123")
  LogRecord::add_attribute(service_a_log, "trace.id", SpanContext::trace_id(Span::span_context(service_a_span)))
  LogRecord::add_attribute(service_a_log, "span.id", SpanContext::span_id(Span::span_context(service_a_span)))
  
  Logger::emit(service_a_logger, service_a_log)
  
  // 模拟跨服务调用，传播上下文
  let service_b_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-b")
  let service_b_span = Tracer::start_span_with_parent(
    service_b_tracer,
    "service-b.validation",
    Span::span_context(service_a_span)
  )
  
  // 服务B记录日志（与span关联）
  let service_b_logger = LoggerProvider::get_logger(LoggerProvider::default(), "service-b")
  let service_b_log = LogRecord::new(Info, "Validating user token")
  LogRecord::add_attribute(service_b_log, "user.id", "user-12345")
  LogRecord::add_attribute(service_b_log, "request.id", "req-abc-123")
  LogRecord::add_attribute(service_b_log, "trace.id", SpanContext::trace_id(Span::span_context(service_b_span)))
  LogRecord::add_attribute(service_b_log, "span.id", SpanContext::span_id(Span::span_context(service_b_span)))
  LogRecord::add_attribute(service_b_log, "parent.span.id", SpanContext::parent_span_id(Span::span_context(service_b_span)))
  
  Logger::emit(service_b_logger, service_b_log)
  
  // 验证日志关联性
  assert_eq(LogRecord::body(service_a_log), Some("Processing user authentication request"))
  assert_eq(LogRecord::body(service_b_log), Some("Validating user token"))
  
  // 验证trace ID一致性
  let service_a_trace_id = LogRecord::get_attribute(service_a_log, "trace.id")
  let service_b_trace_id = LogRecord::get_attribute(service_b_log, "trace.id")
  
  match (service_a_trace_id, service_b_trace_id) {
    (Some(id_a), Some(id_b)) => assert_eq(id_a, id_b)
    _ => assert_true(false)
  }
  
  // 结束span
  Span::end(service_b_span)
  Span::end(service_a_span)
  
  assert_true(true)
}

test "分布式事务追踪" {
  // 模拟分布式事务：用户下单流程
  // 1. 订单服务创建订单
  let order_service_tracer = TracerProvider::get_tracer(TracerProvider::default(), "order-service")
  let order_span = Tracer::start_span(order_service_tracer, "order-service.create-order")
  
  Span::set_attribute(order_span, "service.name", "order-service")
  Span::set_attribute(order_span, "user.id", "user-67890")
  Span::set_attribute(order_span, "order.id", "order-xyz-456")
  Span::set_attribute(order_span, "order.amount", "99.99")
  
  // 2. 支付服务处理支付
  let payment_service_tracer = TracerProvider::get_tracer(TracerProvider::default(), "payment-service")
  let payment_span = Tracer::start_span_with_parent(
    payment_service_tracer,
    "payment-service.process-payment",
    Span::span_context(order_span)
  )
  
  Span::set_attribute(payment_span, "service.name", "payment-service")
  Span::set_attribute(payment_span, "user.id", "user-67890")
  Span::set_attribute(payment_span, "order.id", "order-xyz-456")
  Span::set_attribute(payment_span, "payment.amount", "99.99")
  Span::set_attribute(payment_span, "payment.method", "credit-card")
  
  // 3. 库存服务扣减库存
  let inventory_service_tracer = TracerProvider::get_tracer(TracerProvider::default(), "inventory-service")
  let inventory_span = Tracer::start_span_with_parent(
    inventory_service_tracer,
    "inventory-service.reserve-items",
    Span::span_context(payment_span)
  )
  
  Span::set_attribute(inventory_span, "service.name", "inventory-service")
  Span::set_attribute(inventory_span, "user.id", "user-67890")
  Span::set_attribute(inventory_span, "order.id", "order-xyz-456")
  Span::set_attribute(inventory_span, "item.id", "item-123")
  Span::set_attribute(inventory_span, "quantity", "2")
  
  // 4. 通知服务发送确认
  let notification_service_tracer = TracerProvider::get_tracer(TracerProvider::default(), "notification-service")
  let notification_span = Tracer::start_span_with_parent(
    notification_service_tracer,
    "notification-service.send-confirmation",
    Span::span_context(inventory_span)
  )
  
  Span::set_attribute(notification_span, "service.name", "notification-service")
  Span::set_attribute(notification_span, "user.id", "user-67890")
  Span::set_attribute(notification_span, "order.id", "order-xyz-456")
  Span::set_attribute(notification_span, "notification.type", "email")
  Span::set_attribute(notification_span, "notification.status", "sent")
  
  // 验证分布式事务的追踪链
  let order_context = Span::span_context(order_span)
  let payment_context = Span::span_context(payment_span)
  let inventory_context = Span::span_context(inventory_span)
  let notification_context = Span::span_context(notification_span)
  
  // 验证所有span属于同一个trace
  assert_eq(SpanContext::trace_id(order_context), SpanContext::trace_id(payment_context))
  assert_eq(SpanContext::trace_id(payment_context), SpanContext::trace_id(inventory_context))
  assert_eq(SpanContext::trace_id(inventory_context), SpanContext::trace_id(notification_context))
  
  // 验证父子关系
  assert_eq(SpanContext::span_id(order_context), SpanContext::parent_span_id(payment_context))
  assert_eq(SpanContext::span_id(payment_context), SpanContext::parent_span_id(inventory_context))
  assert_eq(SpanContext::span_id(inventory_context), SpanContext::parent_span_id(notification_context))
  
  // 验证事务一致性属性
  let order_user_id = Span::get_attribute(order_span, "user.id")
  let payment_user_id = Span::get_attribute(payment_span, "user.id")
  let inventory_user_id = Span::get_attribute(inventory_span, "user.id")
  let notification_user_id = Span::get_attribute(notification_span, "user.id")
  
  match (order_user_id, payment_user_id, inventory_user_id, notification_user_id) {
    (Some(uid1), Some(uid2), Some(uid3), Some(uid4)) => {
      assert_eq(uid1, uid2)
      assert_eq(uid2, uid3)
      assert_eq(uid3, uid4)
    }
    _ => assert_true(false)
  }
  
  // 结束所有span（按相反顺序）
  Span::end(notification_span)
  Span::end(inventory_span)
  Span::end(payment_span)
  Span::end(order_span)
  
  assert_true(true)
}

test "跨服务错误传播和处理" {
  // 创建服务A的span
  let service_a_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-a")
  let service_a_span = Tracer::start_span(service_a_tracer, "service-a.operation")
  
  // 服务A记录正常操作
  Span::add_event(service_a_span, "operation.started", [
    ("operation.id", "op-123"),
    ("operation.type", "data-processing")
  ])
  
  // 模拟服务A调用服务B
  let service_b_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-b")
  let service_b_span = Tracer::start_span_with_parent(
    service_b_tracer,
    "service-b.operation",
    Span::span_context(service_a_span)
  )
  
  // 服务B发生错误
  Span::set_status(service_b_span, Error, Some("Database connection failed"))
  Span::add_event(service_b_span, "error.occurred", [
    ("error.type", "DatabaseError"),
    ("error.code", "DB_CONN_FAILED"),
    ("error.message", "Unable to connect to database"),
    ("retry.count", "3")
  ])
  
  // 服务B记录错误日志
  let service_b_logger = LoggerProvider::get_logger(LoggerProvider::default(), "service-b")
  let error_log = LogRecord::new(Error, "Database operation failed")
  LogRecord::add_attribute(error_log, "error.type", "DatabaseError")
  LogRecord::add_attribute(error_log, "error.code", "DB_CONN_FAILED")
  LogRecord::add_attribute(error_log, "trace.id", SpanContext::trace_id(Span::span_context(service_b_span)))
  LogRecord::add_attribute(error_log, "span.id", SpanContext::span_id(Span::span_context(service_b_span)))
  
  Logger::emit(service_b_logger, error_log)
  
  // 错误传播回服务A
  Span::set_status(service_a_span, Error, Some("Dependent service error"))
  Span::add_event(service_a_span, "error.propagated", [
    ("source.service", "service-b"),
    ("error.type", "DatabaseError"),
    ("error.code", "DB_CONN_FAILED")
  ])
  
  // 服务A记录错误度量
  let service_a_meter = MeterProvider::get_meter(MeterProvider::default(), "service-a")
  let error_counter = Meter::create_counter(
    service_a_meter,
    "operation.errors",
    Some("Operation errors"),
    Some("count")
  )
  
  Counter::add_with_attributes(error_counter, 1.0, [
    ("operation.type", "data-processing"),
    ("error.type", "DatabaseError"),
    ("source.service", "service-b")
  ])
  
  // 验证错误传播的追踪链
  let service_a_context = Span::span_context(service_a_span)
  let service_b_context = Span::span_context(service_b_span)
  
  assert_eq(SpanContext::trace_id(service_a_context), SpanContext::trace_id(service_b_context))
  assert_eq(SpanContext::span_id(service_a_context), SpanContext::parent_span_id(service_b_context))
  
  // 验证错误状态
  assert_eq(Span::status_code(service_b_span), Error)
  assert_eq(Span::status_code(service_a_span), Error)
  
  // 结束span
  Span::end(service_b_span)
  Span::end(service_a_span)
  
  assert_true(true)
}

test "跨服务性能监控" {
  // 模拟微服务架构中的性能监控
  let services = ["api-gateway", "user-service", "order-service", "payment-service"]
  let tracers = []
  let spans = []
  
  // 创建各服务的span
  for i in 0..=3 {
    let service_name = services[i]
    let tracer = TracerProvider::get_tracer(TracerProvider::default(), service_name)
    let span_name = service_name + ".process-request"
    
    let span = if i == 0 {
      // 第一个服务（API网关）创建根span
      Tracer::start_span(tracer, span_name)
    } else {
      // 其他服务创建子span
      Tracer::start_span_with_parent(tracer, span_name, Span::span_context(spans[i-1]))
    }
    
    tracers = tracers.push(tracer)
    spans = spans.push(span)
    
    // 设置服务属性
    Span::set_attribute(span, "service.name", service_name)
    Span::set_attribute(span, "request.id", "req-perf-123")
    Span::set_attribute(span, "user.id", "user-perf-456")
  }
  
  // 模拟各服务的处理时间
  let processing_times = [50.5, 120.3, 85.7, 200.1]  // 毫秒
  
  for i in 0..=3 {
    // 模拟处理时间
    @sleep(processing_times[i].to_int())
    
    // 记录处理时间
    let span = spans[i]
    Span::set_attribute(span, "processing.time", processing_times[i])
    
    // 记录服务特定的度量
    let meter = MeterProvider::get_meter(MeterProvider::default(), services[i])
    let latency_histogram = Meter::create_histogram(
      meter,
      "request.latency",
      Some("Request processing latency"),
      Some("milliseconds")
    )
    
    Histogram::record_with_attributes(latency_histogram, processing_times[i], [
      ("service", services[i]),
      ("operation", "process-request"),
      ("endpoint", "/api/process")
    ])
    
    // 记录吞吐量
    let throughput_counter = Meter::create_counter(
      meter,
      "requests.processed",
      Some("Processed requests"),
      Some("count")
    )
    
    Counter::add_with_attributes(throughput_counter, 1.0, [
      ("service", services[i]),
      ("status", "success")
    ])
  }
  
  // 计算跨服务性能指标
  let total_processing_time = processing_times.reduce(fn(acc, time) { acc + time }, 0.0)
  let average_processing_time = total_processing_time / processing_times.length().to_float()
  let max_processing_time = processing_times.reduce(fn(acc, time) { if time > acc { time } else { acc } }, 0.0)
  
  // 记录跨服务性能度量
  let cross_service_meter = MeterProvider::get_meter(MeterProvider::default(), "cross-service")
  let total_latency_histogram = Meter::create_histogram(
    cross_service_meter,
    "total.request.latency",
    Some("Total end-to-end request latency"),
    Some("milliseconds")
  )
  
  Histogram::record(total_latency_histogram, total_processing_time)
  
  // 验证性能指标
  assert_true(total_processing_time > 400.0)  // 总处理时间应大于400ms
  assert_true(average_processing_time > 100.0)  // 平均处理时间应大于100ms
  assert_eq(max_processing_time, 200.1)  // 最大处理时间应为200.1ms
  
  // 验证所有span属于同一个trace
  let root_context = Span::span_context(spans[0])
  for i in 1..=3 {
    let context = Span::span_context(spans[i])
    assert_eq(SpanContext::trace_id(root_context), SpanContext::trace_id(context))
  }
  
  // 结束所有span（按相反顺序）
  for i in [3, 2, 1, 0] {
    Span::end(spans[i])
  }
  
  assert_true(true)
}

test "跨服务配置同步" {
  // 创建配置管理器
  let config_manager = CrossServiceConfigManager::new()
  
  // 注册服务
  let services = ["auth-service", "user-service", "order-service"]
  for service in services {
    CrossServiceConfigManager::register_service(config_manager, service)
  }
  
  // 设置全局配置
  let global_config = [
    ("telemetry.sampling.rate", "0.1"),
    ("telemetry.batch.size", "100"),
    ("telemetry.export.interval", "5000")
  ]
  
  CrossServiceConfigManager::set_global_config(config_manager, global_config)
  
  // 设置服务特定配置
  let auth_service_config = [
    ("auth.token.expiry", "3600"),
    ("auth.max.retry", "3")
  ]
  
  CrossServiceConfigManager::set_service_config(config_manager, "auth-service", auth_service_config)
  
  // 验证配置获取
  let auth_sampling_rate = CrossServiceConfigManager::get_config(
    config_manager, 
    "auth-service", 
    "telemetry.sampling.rate"
  )
  
  let auth_token_expiry = CrossServiceConfigManager::get_config(
    config_manager, 
    "auth-service", 
    "auth.token.expiry"
  )
  
  let user_sampling_rate = CrossServiceConfigManager::get_config(
    config_manager, 
    "user-service", 
    "telemetry.sampling.rate"
  )
  
  // 验证配置值
  match auth_sampling_rate {
    Some(value) => assert_eq(value, "0.1")
    None => assert_true(false)
  }
  
  match auth_token_expiry {
    Some(value) => assert_eq(value, "3600")
    None => assert_true(false)
  }
  
  match user_sampling_rate {
    Some(value) => assert_eq(value, "0.1")  // 应该回退到全局配置
    None => assert_true(false)
  }
  
  // 测试配置变更通知
  let notification_received = @ref(false)
  
  CrossServiceConfigManager::subscribe_to_changes(
    config_manager,
    "telemetry.sampling.rate",
    fn(service, key, old_value, new_value) {
      notification_received := true
      assert_eq(key, "telemetry.sampling.rate")
      assert_eq(old_value, "0.1")
      assert_eq(new_value, "0.2")
    }
  )
  
  // 更新配置
  CrossServiceConfigManager::update_global_config(
    config_manager,
    "telemetry.sampling.rate",
    "0.2"
  )
  
  // 验证通知被触发
  assert_true(notification_received)
  
  // 验证配置已更新
  let updated_sampling_rate = CrossServiceConfigManager::get_config(
    config_manager,
    "auth-service",
    "telemetry.sampling.rate"
  )
  
  match updated_sampling_rate {
    Some(value) => assert_eq(value, "0.2")
    None => assert_true(false)
  }
}