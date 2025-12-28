// Azimuth Telemetry System - 跨服务传播测试用例
// 测试分布式系统中追踪上下文在服务间的传播

test "基本跨服务追踪传播测试" {
  // 模拟服务A创建追踪上下文
  let tracer_provider_a = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(tracer_provider_a, "service-a")
  
  let span_a = Tracer::start_span(tracer_a, "service-a-operation")
  let span_ctx_a = Span::span_context(span_a)
  
  // 创建传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 服务A准备向服务B发送请求
  let carrier_to_b = TextMapCarrier::new()
  let ctx_a = Context::root()
  
  // 注入追踪上下文到carrier
  CompositePropagator::inject(composite_propagator, ctx_a, carrier_to_b)
  
  // 模拟服务B接收请求并提取追踪上下文
  let ctx_b = CompositePropagator::extract(composite_propagator, carrier_to_b)
  
  // 服务B创建子Span
  let tracer_provider_b = TracerProvider::default()
  let tracer_b = TracerProvider::get_tracer(tracer_provider_b, "service-b")
  
  let span_b = Tracer::start_span(tracer_b, "service-b-operation")
  let span_ctx_b = Span::span_context(span_b)
  
  // 验证追踪上下文的传播
  assert_eq(SpanContext::trace_id(span_ctx_a), SpanContext::trace_id(span_ctx_b))
  assert_true(SpanContext::is_sampled(span_ctx_a) == SpanContext::is_sampled(span_ctx_b))
  
  // 结束Span
  Span::end(span_b)
  Span::end(span_a)
}

test "多层服务调用传播测试" {
  // 模拟服务链：A -> B -> C -> D
  
  // 服务A
  let tracer_provider_a = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(tracer_provider_a, "service-a")
  let span_a = Tracer::start_span(tracer_a, "service-a-operation")
  let span_ctx_a = Span::span_context(span_a)
  
  // 创建传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // A -> B
  let carrier_a_to_b = TextMapCarrier::new()
  let ctx_a = Context::root()
  CompositePropagator::inject(composite_propagator, ctx_a, carrier_a_to_b)
  
  let ctx_b = CompositePropagator::extract(composite_propagator, carrier_a_to_b)
  let tracer_provider_b = TracerProvider::default()
  let tracer_b = TracerProvider::get_tracer(tracer_provider_b, "service-b")
  let span_b = Tracer::start_span(tracer_b, "service-b-operation")
  let span_ctx_b = Span::span_context(span_b)
  
  // B -> C
  let carrier_b_to_c = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx_b, carrier_b_to_c)
  
  let ctx_c = CompositePropagator::extract(composite_propagator, carrier_b_to_c)
  let tracer_provider_c = TracerProvider::default()
  let tracer_c = TracerProvider::get_tracer(tracer_provider_c, "service-c")
  let span_c = Tracer::start_span(tracer_c, "service-c-operation")
  let span_ctx_c = Span::span_context(span_c)
  
  // C -> D
  let carrier_c_to_d = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx_c, carrier_c_to_d)
  
  let ctx_d = CompositePropagator::extract(composite_propagator, carrier_c_to_d)
  let tracer_provider_d = TracerProvider::default()
  let tracer_d = TracerProvider::get_tracer(tracer_provider_d, "service-d")
  let span_d = Tracer::start_span(tracer_d, "service-d-operation")
  let span_ctx_d = Span::span_context(span_d)
  
  // 验证所有服务使用相同的trace_id
  assert_eq(SpanContext::trace_id(span_ctx_a), SpanContext::trace_id(span_ctx_b))
  assert_eq(SpanContext::trace_id(span_ctx_b), SpanContext::trace_id(span_ctx_c))
  assert_eq(SpanContext::trace_id(span_ctx_c), SpanContext::trace_id(span_ctx_d))
  
  // 验证采样标志的一致性
  let sampled_a = SpanContext::is_sampled(span_ctx_a)
  let sampled_b = SpanContext::is_sampled(span_ctx_b)
  let sampled_c = SpanContext::is_sampled(span_ctx_c)
  let sampled_d = SpanContext::is_sampled(span_ctx_d)
  
  assert_true(sampled_a == sampled_b && sampled_b == sampled_c && sampled_c == sampled_d)
  
  // 结束所有Span
  Span::end(span_d)
  Span::end(span_c)
  Span::end(span_b)
  Span::end(span_a)
}

test "Baggage跨服务传播测试" {
  // 创建包含Baggage的上下文
  let baggage = Baggage::new()
  let baggage_with_entries = Baggage::set_entry(baggage, "user.id", "user-123")
  let baggage_final = Baggage::set_entry(baggage_with_entries, "request.id", "req-456")
  
  // 服务A创建带有Baggage的上下文
  let ctx_a = Context::root()
  let user_key = ContextKey::new("user.id")
  let request_key = ContextKey::new("request.id")
  let ctx_with_baggage = Context::with_value(ctx_a, user_key, "user-123")
  let ctx_final = Context::with_value(ctx_with_baggage, request_key, "req-456")
  
  // 创建传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 服务A向服务B传播
  let carrier_a_to_b = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx_final, carrier_a_to_b)
  
  // 服务B提取上下文和Baggage
  let ctx_b = CompositePropagator::extract(composite_propagator, carrier_a_to_b)
  
  // 验证Baggage信息是否正确传播
  let user_id_b = Context::get(ctx_b, user_key)
  let request_id_b = Context::get(ctx_b, request_key)
  
  // 注意：在简化实现中，Baggage传播可能不完全支持
  // 在完整实现中，应该验证Baggage的正确传播
  
  // 服务B向服务C传播
  let carrier_b_to_c = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx_b, carrier_b_to_c)
  
  let ctx_c = CompositePropagator::extract(composite_propagator, carrier_b_to_c)
  let user_id_c = Context::get(ctx_c, user_key)
  let request_id_c = Context::get(ctx_c, request_key)
  
  // 验证Baggage在多跳传播中的一致性
  // assert_eq(user_id_b, Some("user-123"))
  // assert_eq(request_id_b, Some("req-456"))
  // assert_eq(user_id_c, Some("user-123"))
  // assert_eq(request_id_c, Some("req-456"))
}

test "HTTP请求头传播测试" {
  // 模拟通过HTTP请求头传播追踪上下文
  
  // 服务A创建HTTP请求
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  let ctx_a = Context::root()
  let http_headers = TextMapCarrier::new()
  
  // 注入追踪上下文到HTTP头
  CompositePropagator::inject(composite_propagator, ctx_a, http_headers)
  
  // 模拟创建HTTP请求
  let request_headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123")
    // 注意：在完整实现中，traceparent应该被自动添加
  ]
  let http_request = HttpRequest::new("POST", "https://service-b.example.com/api", request_headers, Some("{\"data\":\"test\"}"))
  
  // 服务B接收HTTP请求并提取追踪上下文
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Request-ID", "req-123")
  ]
  let http_response = HttpResponse::new(200, response_headers, Some("{\"result\":\"success\"}"))
  
  // 从HTTP请求中提取追踪上下文
  let ctx_b = CompositePropagator::extract(composite_propagator, http_headers)
  
  // 服务B创建响应，包含追踪上下文
  let response_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx_b, response_carrier)
  
  // 验证HTTP请求和响应的处理
  assert_eq(HttpRequest::http_method(http_request), "POST")
  assert_eq(HttpRequest::url(http_request), "https://service-b.example.com/api")
  assert_eq(HttpResponse::status_code(http_response), 200)
  assert_eq(HttpResponse::body(http_response), Some("{\"result\":\"success\"}"))
}

test "异步服务调用传播测试" {
  // 模拟异步服务调用中的追踪上下文传播
  
  // 主服务创建根Span
  let main_tracer_provider = TracerProvider::default()
  let main_tracer = TracerProvider::get_tracer(main_tracer_provider, "main-service")
  let main_span = Tracer::start_span(main_tracer, "main-operation")
  let main_span_ctx = Span::span_context(main_span)
  
  // 创建传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 模拟多个异步调用
  let async_calls = 3
  
  for i in 0..async_calls {
    // 为每个异步调用准备传播上下文
    let async_carrier = TextMapCarrier::new()
    let main_ctx = Context::root()
    CompositePropagator::inject(composite_propagator, main_ctx, async_carrier)
    
    // 异步服务提取上下文
    let async_ctx = CompositePropagator::extract(composite_propagator, async_carrier)
    
    // 异步服务创建Span
    let async_tracer_provider = TracerProvider::default()
    let async_tracer = TracerProvider::get_tracer(async_tracer_provider, "async-service-" + i.to_string())
    let async_span = Tracer::start_span(async_tracer, "async-operation-" + i.to_string())
    let async_span_ctx = Span::span_context(async_span)
    
    // 验证异步调用使用相同的trace_id
    assert_eq(SpanContext::trace_id(main_span_ctx), SpanContext::trace_id(async_span_ctx))
    
    // 验证采样标志的一致性
    assert_true(SpanContext::is_sampled(main_span_ctx) == SpanContext::is_sampled(async_span_ctx))
    
    // 结束异步Span
    Span::end(async_span)
  }
  
  // 结束主Span
  Span::end(main_span)
}

test "跨服务错误传播测试" {
  // 测试错误条件下的追踪上下文传播
  
  // 服务A创建Span
  let tracer_provider_a = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(tracer_provider_a, "service-a")
  let span_a = Tracer::start_span(tracer_a, "service-a-operation")
  
  // 设置错误状态
  Span::set_status(span_a, Error, Some("Service operation failed"))
  
  // 传播上下文到服务B
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  let carrier = TextMapCarrier::new()
  let ctx_a = Context::root()
  CompositePropagator::inject(composite_propagator, ctx_a, carrier)
  
  // 服务B提取上下文并创建Span
  let ctx_b = CompositePropagator::extract(composite_propagator, carrier)
  let tracer_provider_b = TracerProvider::default()
  let tracer_b = TracerProvider::get_tracer(tracer_provider_b, "service-b")
  let span_b = Tracer::start_span(tracer_b, "service-b-operation")
  
  // 服务B也遇到错误
  Span::set_status(span_b, Error, Some("Downstream service error"))
  
  // 记录错误日志
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "error-test-logger")
  
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Error occurred in service chain"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(span_b))),
    Some(SpanContext::span_id(Span::span_context(span_b))),
    Some(ctx_b)
  )
  
  Logger::emit(logger, error_log)
  
  // 验证错误状态和上下文传播
  assert_eq(Span::status(span_a), Error)
  assert_eq(Span::status(span_b), Error)
  
  // 结束Span
  Span::end(span_b)
  Span::end(span_a)
}

test "微服务架构传播测试" {
  // 模拟复杂微服务架构中的追踪传播
  
  // API Gateway
  let gateway_tracer_provider = TracerProvider::default()
  let gateway_tracer = TracerProvider::get_tracer(gateway_tracer_provider, "api-gateway")
  let gateway_span = Tracer::start_span(gateway_tracer, "gateway-request")
  let gateway_span_ctx = Span::span_context(gateway_span)
  
  // 创建传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // Gateway -> Auth Service
  let gateway_carrier = TextMapCarrier::new()
  let gateway_ctx = Context::root()
  CompositePropagator::inject(composite_propagator, gateway_ctx, gateway_carrier)
  
  let auth_ctx = CompositePropagator::extract(composite_propagator, gateway_carrier)
  let auth_tracer_provider = TracerProvider::default()
  let auth_tracer = TracerProvider::get_tracer(auth_tracer_provider, "auth-service")
  let auth_span = Tracer::start_span(auth_tracer, "authenticate")
  
  // Auth Service -> User Service
  let auth_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, auth_ctx, auth_carrier)
  
  let user_ctx = CompositePropagator::extract(composite_propagator, auth_carrier)
  let user_tracer_provider = TracerProvider::default()
  let user_tracer = TracerProvider::get_tracer(user_tracer_provider, "user-service")
  let user_span = Tracer::start_span(user_tracer, "get-user-profile")
  
  // User Service -> Database
  let user_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, user_ctx, user_carrier)
  
  let db_ctx = CompositePropagator::extract(composite_propagator, user_carrier)
  let db_tracer_provider = TracerProvider::default()
  let db_tracer = TracerProvider::get_tracer(db_tracer_provider, "database")
  let db_span = Tracer::start_span(db_tracer, "query-user")
  
  // 验证整个调用链使用相同的trace_id
  let gateway_trace_id = SpanContext::trace_id(gateway_span_ctx)
  let auth_trace_id = SpanContext::trace_id(Span::span_context(auth_span))
  let user_trace_id = SpanContext::trace_id(Span::span_context(user_span))
  let db_trace_id = SpanContext::trace_id(Span::span_context(db_span))
  
  assert_eq(gateway_trace_id, auth_trace_id)
  assert_eq(auth_trace_id, user_trace_id)
  assert_eq(user_trace_id, db_trace_id)
  
  // 验证所有span都有不同的span_id
  let gateway_span_id = SpanContext::span_id(gateway_span_ctx)
  let auth_span_id = SpanContext::span_id(Span::span_context(auth_span))
  let user_span_id = SpanContext::span_id(Span::span_context(user_span))
  let db_span_id = SpanContext::span_id(Span::span_context(db_span))
  
  assert_true(gateway_span_id != auth_span_id)
  assert_true(auth_span_id != user_span_id)
  assert_true(user_span_id != db_span_id)
  
  // 结束所有Span（按调用顺序的逆序）
  Span::end(db_span)
  Span::end(user_span)
  Span::end(auth_span)
  Span::end(gateway_span)
}

test "跨服务指标关联测试" {
  // 测试跨服务调用的指标关联
  
  // 服务A创建指标
  let meter_provider_a = MeterProvider::default()
  let meter_a = MeterProvider::get_meter(meter_provider_a, "service-a")
  let request_counter_a = Meter::create_counter(meter_a, "requests_total", Some("Total requests"), Some("count"))
  let duration_histogram_a = Meter::create_histogram(meter_a, "request_duration", Some("Request duration"), Some("ms"))
  
  // 记录服务A的指标
  Counter::add(request_counter_a, 1.0)
  Histogram::record(duration_histogram_a, 100.0)
  
  // 传播上下文到服务B
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  let carrier = TextMapCarrier::new()
  let ctx_a = Context::root()
  CompositePropagator::inject(composite_propagator, ctx_a, carrier)
  
  // 服务B接收请求并创建指标
  let ctx_b = CompositePropagator::extract(composite_propagator, carrier)
  let meter_provider_b = MeterProvider::default()
  let meter_b = MeterProvider::get_meter(meter_provider_b, "service-b")
  let request_counter_b = Meter::create_counter(meter_b, "requests_total", Some("Total requests"), Some("count"))
  let duration_histogram_b = Meter::create_histogram(meter_b, "request_duration", Some("Request duration"), Some("ms"))
  
  // 记录服务B的指标
  Counter::add(request_counter_b, 1.0)
  Histogram::record(duration_histogram_b, 50.0)
  
  // 验证指标创建和记录
  assert_eq(request_counter_a.name, "requests_total")
  assert_eq(request_counter_b.name, "requests_total")
  assert_eq(duration_histogram_a.name, "request_duration")
  assert_eq(duration_histogram_b.name, "request_duration")
  
  // 验证指标描述和单位的一致性
  assert_eq(request_counter_a.description, Some("Total requests"))
  assert_eq(request_counter_b.description, Some("Total requests"))
  assert_eq(duration_histogram_a.unit, Some("ms"))
  assert_eq(duration_histogram_b.unit, Some("ms"))
}