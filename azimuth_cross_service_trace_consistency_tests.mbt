// Azimuth 跨服务追踪一致性测试用例
// 专注于分布式系统中追踪信息的一致性和完整性

// 测试1: 跨服务追踪上下文传递一致性
test "跨服务追踪上下文传递一致性测试" {
  // 创建初始追踪上下文
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "service.a")
  
  let root_span = Tracer::start_span(tracer, "api.entry.point")
  let root_context = Span::span_context(root_span)
  
  // 验证根span上下文
  assert_true(SpanContext::is_valid(root_context))
  assert_true(SpanContext::is_sampled(root_context))
  assert_eq(SpanContext::trace_id(root_context), "test_trace_id")
  
  // 模拟服务间调用 - 传递追踪上下文
  let service_b_tracer = TracerProvider::get_tracer(tracer_provider, "service.b")
  let child_span = Tracer::start_span(service_b_tracer, "database.operation")
  
  // 验证子span与父span的追踪ID一致
  let child_context = Span::span_context(child_span)
  assert_eq(SpanContext::trace_id(child_context), SpanContext::trace_id(root_context))
  
  // 模拟服务链调用
  let service_c_tracer = TracerProvider::get_tracer(tracer_provider, "service.c")
  let grandchild_span = Tracer::start_span(service_c_tracer, "cache.operation")
  
  // 验证整个调用链的追踪ID一致性
  let grandchild_context = Span::span_context(grandchild_span)
  assert_eq(SpanContext::trace_id(grandchild_context), SpanContext::trace_id(root_context))
  
  // 完成所有span
  Span::end(grandchild_span)
  Span::end(child_span)
  Span::end(root_span)
}

// 测试2: 跨服务Baggage传递一致性
test "跨服务Baggage传递一致性测试" {
  // 创建根上下文并添加baggage
  let root_ctx = Context::root()
  let user_id_key = ContextKey::new("user.id")
  let request_id_key = ContextKey::new("request.id")
  let tenant_key = ContextKey::new("tenant.id")
  
  let ctx_with_baggage = root_ctx
    |> Context::with_value(user_id_key, "user-12345")
    |> Context::with_value(request_id_key, "req-abcde")
    |> Context::with_value(tenant_key, "tenant-prod")
  
  // 创建传播器并注入上下文
  let propagator = W3CTraceContextPropagator::new()
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(CompositePropagator::new([propagator]), ctx_with_baggage, carrier)
  
  // 验证baggage项在上下文中
  assert_eq(Context::get(ctx_with_baggage, user_id_key), Some("user-12345"))
  assert_eq(Context::get(ctx_with_baggage, request_id_key), Some("req-abcde"))
  assert_eq(Context::get(ctx_with_baggage, tenant_key), Some("tenant-prod"))
  
  // 模拟跨服务提取上下文
  let extracted_ctx = CompositePropagator::extract(CompositePropagator::new([propagator]), carrier)
  
  // 验证提取的上下文包含原始baggage项
  let extracted_user_id = Context::get(extracted_ctx, user_id_key)
  let extracted_request_id = Context::get(extracted_ctx, request_id_key)
  let extracted_tenant = Context::get(extracted_ctx, tenant_key)
  
  assert_eq(extracted_user_id, Some("user-12345"))
  assert_eq(extracted_request_id, Some("req-abcde"))
  assert_eq(extracted_tenant, Some("tenant-prod"))
}

// 测试3: 跨服务Span层次关系一致性
test "跨服务Span层次关系一致性测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "gateway.service")
  
  // 创建网关层的根span
  let gateway_span = Tracer::start_span(tracer, "gateway.request")
  let gateway_context = Span::span_context(gateway_span)
  
  // 创建业务服务层span
  let business_tracer = TracerProvider::get_tracer(tracer_provider, "business.service")
  let business_span = Tracer::start_span(business_tracer, "business.logic")
  let business_context = Span::span_context(business_span)
  
  // 创建数据访问层span
  let data_tracer = TracerProvider::get_tracer(tracer_provider, "data.service")
  let data_span = Tracer::start_span(data_tracer, "data.access")
  let data_context = Span::span_context(data_span)
  
  // 验证所有span属于同一追踪
  assert_eq(SpanContext::trace_id(gateway_context), SpanContext::trace_id(business_context))
  assert_eq(SpanContext::trace_id(business_context), SpanContext::trace_id(data_context))
  
  // 验证span ID的唯一性
  assert_ne(SpanContext::span_id(gateway_context), SpanContext::span_id(business_context))
  assert_ne(SpanContext::span_id(business_context), SpanContext::span_id(data_context))
  assert_ne(SpanContext::span_id(gateway_context), SpanContext::span_id(data_context))
  
  // 验证span状态
  assert_eq(Span::status(gateway_span), Unset)
  assert_eq(Span::status(business_span), Unset)
  assert_eq(Span::status(data_span), Unset)
  
  // 设置span状态并完成
  Span::set_status(data_span, Ok)
  Span::set_status(business_span, Ok)
  Span::set_status(gateway_span, Ok)
  
  Span::end(data_span)
  Span::end(business_span)
  Span::end(gateway_span)
  
  // 验证完成后的状态
  assert_eq(Span::status(data_span), Ok)
  assert_eq(Span::status(business_span), Ok)
  assert_eq(Span::status(gateway_span), Ok)
}

// 测试4: 跨服务错误传播一致性
test "跨服务错误传播一致性测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error.prone.service")
  
  // 创建正常操作的span
  let normal_span = Tracer::start_span(tracer, "normal.operation")
  Span::set_status(normal_span, Ok)
  Span::end(normal_span)
  
  // 创建错误操作的span
  let error_span = Tracer::start_span(tracer, "error.operation")
  Span::set_status(error_span, Error)
  
  // 添加错误事件
  Span::add_event(error_span, "exception.occurred", None)
  
  // 创建恢复操作的span
  let recovery_span = Tracer::start_span(tracer, "recovery.operation")
  Span::set_status(recovery_span, Ok)
  Span::end(recovery_span)
  
  // 完成错误span
  Span::end(error_span)
  
  // 验证状态传播
  assert_eq(Span::status(normal_span), Ok)
  assert_eq(Span::status(error_span), Error)
  assert_eq(Span::status(recovery_span), Ok)
  
  // 验证span上下文一致性
  let normal_context = Span::span_context(normal_span)
  let error_context = Span::span_context(error_span)
  let recovery_context = Span::span_context(recovery_span)
  
  assert_eq(SpanContext::trace_id(normal_context), SpanContext::trace_id(error_context))
  assert_eq(SpanContext::trace_id(error_context), SpanContext::trace_id(recovery_context))
}

// 测试5: 跨服务时间戳一致性
test "跨服务时间戳一致性测试" {
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // 创建日志记录器
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "timestamp.consistency.logger")
  
  // 在不同服务中创建日志记录
  let service_a_log = LogRecord::new_with_context(
    Info,
    Some("Service A operation started"),
    None,
    Some(base_timestamp),
    Some(base_timestamp + 1000L),
    Some("trace-a123"),
    Some("span-a456"),
    None
  )
  
  let service_b_log = LogRecord::new_with_context(
    Info,
    Some("Service B operation started"),
    None,
    Some(base_timestamp + 5000L),
    Some(base_timestamp + 6000L),
    Some("trace-a123"),
    Some("span-b789"),
    None
  )
  
  let service_c_log = LogRecord::new_with_context(
    Info,
    Some("Service C operation started"),
    None,
    Some(base_timestamp + 10000L),
    Some(base_timestamp + 11000L),
    Some("trace-a123"),
    Some("span-c012"),
    None
  )
  
  // 验证时间戳的递增性
  assert_true(LogRecord::timestamp(service_a_log) < LogRecord::timestamp(service_b_log))
  assert_true(LogRecord::timestamp(service_b_log) < LogRecord::timestamp(service_c_log))
  
  // 验证追踪ID一致性
  assert_eq(LogRecord::trace_id(service_a_log), Some("trace-a123"))
  assert_eq(LogRecord::trace_id(service_b_log), Some("trace-a123"))
  assert_eq(LogRecord::trace_id(service_c_log), Some("trace-a123"))
  
  // 验证span ID唯一性
  assert_ne(LogRecord::span_id(service_a_log), LogRecord::span_id(service_b_log))
  assert_ne(LogRecord::span_id(service_b_log), LogRecord::span_id(service_c_log))
  assert_ne(LogRecord::span_id(service_a_log), LogRecord::span_id(service_c_log))
  
  // 发送日志记录
  Logger::emit(logger, service_a_log)
  Logger::emit(logger, service_b_log)
  Logger::emit(logger, service_c_log)
}

// 测试6: 跨服务资源属性一致性
test "跨服务资源属性一致性测试" {
  // 创建基础资源
  let base_resource = Resource::new()
  
  // 添加全局服务属性
  let global_attrs = [
    ("service.namespace", StringValue("production")),
    ("service.environment", StringValue("prod")),
    ("service.version", StringValue("2.1.0"))
  ]
  let global_resource = Resource::with_attributes(base_resource, global_attrs)
  
  // 添加服务A特定属性
  let service_a_attrs = [
    ("service.name", StringValue("auth.service")),
    ("service.instance.id", StringValue("auth-prod-01")),
    ("service.port", IntValue(8080))
  ]
  let service_a_resource = Resource::with_attributes(global_resource, service_a_attrs)
  
  // 添加服务B特定属性
  let service_b_attrs = [
    ("service.name", StringValue("payment.service")),
    ("service.instance.id", StringValue("payment-prod-02")),
    ("service.port", IntValue(8081))
  ]
  let service_b_resource = Resource::with_attributes(global_resource, service_b_attrs)
  
  // 验证全局属性在所有服务中存在
  assert_eq(Resource::get_attribute(service_a_resource, "service.namespace"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(service_b_resource, "service.namespace"), Some(StringValue("production")))
  
  assert_eq(Resource::get_attribute(service_a_resource, "service.environment"), Some(StringValue("prod")))
  assert_eq(Resource::get_attribute(service_b_resource, "service.environment"), Some(StringValue("prod")))
  
  assert_eq(Resource::get_attribute(service_a_resource, "service.version"), Some(StringValue("2.1.0")))
  assert_eq(Resource::get_attribute(service_b_resource, "service.version"), Some(StringValue("2.1.0")))
  
  // 验证服务特定属性
  assert_eq(Resource::get_attribute(service_a_resource, "service.name"), Some(StringValue("auth.service")))
  assert_eq(Resource::get_attribute(service_b_resource, "service.name"), Some(StringValue("payment.service")))
  
  assert_eq(Resource::get_attribute(service_a_resource, "service.instance.id"), Some(StringValue("auth-prod-01")))
  assert_eq(Resource::get_attribute(service_b_resource, "service.instance.id"), Some(StringValue("payment-prod-02")))
}

// 测试7: 跨服务指标一致性
test "跨服务指标一致性测试" {
  let provider = MeterProvider::default()
  
  // 创建服务A的指标
  let service_a_meter = MeterProvider::get_meter(provider, "auth.service")
  let auth_requests = Meter::create_counter(service_a_meter, "auth.requests.total")
  let auth_latency = Meter::create_histogram(service_a_meter, "auth.request.duration")
  
  // 创建服务B的指标
  let service_b_meter = MeterProvider::get_meter(provider, "payment.service")
  let payment_requests = Meter::create_counter(service_b_meter, "payment.requests.total")
  let payment_latency = Meter::create_histogram(service_b_meter, "payment.request.duration")
  
  // 记录指标数据
  Counter::add(auth_requests, 1000.0)
  Histogram::record(auth_latency, 0.125)
  Histogram::record(auth_latency, 0.250)
  
  Counter::add(payment_requests, 500.0)
  Histogram::record(payment_latency, 0.350)
  Histogram::record(payment_latency, 0.450)
  
  // 验证指标名称一致性
  assert_eq(Instrument::name(Counter(auth_requests.name, None, None)), "auth.requests.total")
  assert_eq(Instrument::name(Counter(payment_requests.name, None, None)), "payment.requests.total")
  
  // 验证指标类型一致性
  match Counter(auth_requests.name, None, None) {
    Counter(name, _, _) => assert_eq(name, "auth.requests.total")
    _ => assert_true(false)
  }
  
  match Histogram(payment_latency.name, None, None) {
    Histogram(name, _, _) => assert_eq(name, "payment.request.duration")
    _ => assert_true(false)
  }
}

// 测试8: 跨服务传播器一致性
test "跨服务传播器一致性测试" {
  // 创建复合传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 创建源上下文
  let source_ctx = Context::root()
  let trace_key = ContextKey::new("trace.info")
  let metadata_key = ContextKey::new("metadata")
  
  let enriched_ctx = source_ctx
    |> Context::with_value(trace_key, "sample.trace")
    |> Context::with_value(metadata_key, "sample.metadata")
  
  // 创建载体并注入上下文
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, enriched_ctx, carrier)
  
  // 验证载体包含追踪信息
  let trace_header = TextMapCarrier::get(carrier, "traceparent")
  assert_true(trace_header.is_some())
  
  // 提取上下文
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // 验证提取的上下文包含预期信息
  let extracted_trace = Context::get(extracted_ctx, trace_key)
  let extracted_metadata = Context::get(extracted_ctx, metadata_key)
  
  assert_eq(extracted_trace, Some("sample.trace"))
  assert_eq(extracted_metadata, Some("sample.metadata"))
}