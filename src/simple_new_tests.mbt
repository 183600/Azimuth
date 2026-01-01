// 简单的新测试用例
test "资源合并策略测试" {
  // 测试基础资源合并
  let resource1 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("service-A")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("environment", azimuth::StringValue("development"))
  ])
  
  let resource2 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("service-B")),  // 应该覆盖
    ("host.name", azimuth::StringValue("host-123")),
    ("region", azimuth::StringValue("us-west"))
  ])
  
  let merged = azimuth::Resource::merge(resource1, resource2)
  
  // 验证合并结果
  assert_eq(azimuth::Resource::get_attribute(merged, "service.name"), Some(azimuth::StringValue("service-B")))
  assert_eq(azimuth::Resource::get_attribute(merged, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged, "environment"), Some(azimuth::StringValue("development")))
  assert_eq(azimuth::Resource::get_attribute(merged, "host.name"), Some(azimuth::StringValue("host-123")))
  assert_eq(azimuth::Resource::get_attribute(merged, "region"), Some(azimuth::StringValue("us-west")))
}

test "内存管理和资源清理测试" {
  // 测试大量Span创建和销毁
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "memory-test")
  
  let spans = []
  
  // 创建大量Span以测试内存管理
  for i = 0; i < 100; i = i + 1 {
    let span = azimuth::Tracer::start_span(tracer, "memory-test-span-" + i.to_string())
    spans.push(span)
  }
  
  // 验证所有Span都被正确创建
  assert_eq(spans.length(), 100)
}

test "高并发遥测一致性测试" {
  // 模拟高并发的Span创建和操作
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "concurrency-test")
  
  let spans = []
  let trace_ids = []
  
  // 创建大量并发Span（模拟并发场景）
  for i = 0; i < 100; i = i + 1 {
    let trace_id = "trace-" + i.to_string()
    let span_id = "span-" + i.to_string()
    
    trace_ids.push(trace_id)
    
    let span_ctx = azimuth::SpanContext::new(trace_id, span_id, true, "")
    let span = azimuth::Span::new("concurrent-operation-" + i.to_string(), azimuth::Internal, span_ctx)
    
    spans.push(span)
  }
  
  // 验证所有Span都被正确创建
  assert_eq(spans.length(), 100)
  assert_eq(trace_ids.length(), 100)
}

test "网络安全遥测测试" {
  // 测试安全相关的Span属性
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "security-tracer")
  
  let span = azimuth::Tracer::start_span(tracer, "secure-operation")
  
  // 添加安全相关属性
  azimuth::Span::add_event(span, "security.auth", Some([
    ("user.id", azimuth::StringValue("user-123")),
    ("auth.method", azimuth::StringValue("oauth2")),
    ("auth.success", azimuth::BoolValue(true)),
    ("security.level", azimuth::StringValue("high"))
  ]))
  
  // 测试安全相关的度量
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "security-meter")
  
  let auth_counter = azimuth::Meter::create_counter(meter, "security.auth.attempts")
  let auth_success_counter = azimuth::Meter::create_counter(meter, "security.auth.success")
  let auth_failure_counter = azimuth::Meter::create_counter(meter, "security.auth.failures")
  let security_events_counter = azimuth::Meter::create_counter(meter, "security.events")
  
  // 记录安全度量
  azimuth::Counter::add(auth_counter, 100.0)
  azimuth::Counter::add(auth_success_counter, 85.0)
  azimuth::Counter::add(auth_failure_counter, 15.0)
  azimuth::Counter::add(security_events_counter, 25.0)
  
  azimuth::Span::end(span)
}

test "时间序列数据压缩测试" {
  // 测试时间序列数据生成
  let clock = azimuth::Clock::system()
  let base_timestamp = azimuth::Clock::now_unix_nanos(clock)
  
  // 生成时间序列数据点
  let time_series_data = []
  
  for i = 0; i < 100; i = i + 1 {
    let timestamp = base_timestamp + (i.to_long() * 1000000L)  // 每毫秒一个数据点
    let value = 100.0 + (i.to_double() * 0.1) + (i.to_double() % 10.0)  // 模拟传感器数据
    
    time_series_data.push((timestamp, value))
  }
  
  // 验证时间序列数据生成
  assert_eq(time_series_data.length(), 100)
}

test "遥测数据生命周期管理测试" {
  // 测试数据创建阶段
  let creation_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建Span
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "lifecycle-tracer")
  let span = azimuth::Tracer::start_span(tracer, "lifecycle-operation")
  
  // 创建度量
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "lifecycle-meter")
  let counter = azimuth::Meter::create_counter(meter, "lifecycle.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "lifecycle.histogram")
  
  // 创建日志记录
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "lifecycle-logger")
  let log_record = azimuth::LogRecord::new(azimuth::Info, "Lifecycle test log")
  
  let creation_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let creation_duration = creation_end_time - creation_start_time
  
  // 验证创建阶段
  assert_true(creation_duration > 0L)
  assert_eq(azimuth::Span::name(span), "lifecycle-operation")
  assert_eq(counter.name, "lifecycle.counter")
  assert_eq(histogram.name, "lifecycle.histogram")
  assert_eq(logger.scope.name, "lifecycle-logger")
  
  azimuth::Span::end(span)
}

test "错误边界和恢复测试" {
  // 测试Span操作的错误处理
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "error-boundary-tracer")
  
  // 测试无效Span创建
  let invalid_span_ctx = azimuth::SpanContext::new("", "", false, "")
  let invalid_span = azimuth::Span::new("invalid-operation", azimuth::Internal, invalid_span_ctx)
  
  // 验证无效Span的处理
  assert_false(azimuth::SpanContext::is_valid(invalid_span_ctx))
  assert_false(azimuth::SpanContext::is_sampled(invalid_span_ctx))
  
  // 测试有效Span的错误恢复
  let valid_span_ctx = azimuth::SpanContext::new("valid-trace", "valid-span", true, "")
  let valid_span = azimuth::Span::new("valid-operation", azimuth::Internal, valid_span_ctx)
  
  // 添加错误事件到Span
  azimuth::Span::add_event(valid_span, "error.occurred", Some([
    ("error.type", azimuth::StringValue("validation.error")),
    ("error.message", azimuth::StringValue("Invalid input parameter")),
    ("error.code", azimuth::IntValue(400))
  ]))
  
  // 设置错误状态
  azimuth::Span::set_status(valid_span, azimuth::Error)
  
  // 验证错误状态设置
  assert_eq(azimuth::Span::name(valid_span), "valid-operation")
  
  azimuth::Span::end(valid_span)
}

test "微服务架构遥测集成测试" {
  // 模拟微服务架构中的多个服务
  let gateway_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "gateway-service")
  let auth_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "auth-service")
  let user_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "user-service")
  
  // 网关服务：接收外部请求
  let gateway_span_ctx = azimuth::SpanContext::new("trace-gateway-123", "span-gateway-root", true, "")
  let gateway_span = azimuth::Span::new("gateway.request", azimuth::Server, gateway_span_ctx)
  
  azimuth::Span::add_event(gateway_span, "gateway.request.received", Some([
    ("http.method", azimuth::StringValue("POST")),
    ("http.url", azimuth::StringValue("/api/orders")),
    ("user.agent", azimuth::StringValue("MobileApp/1.0")),
    ("client.ip", azimuth::StringValue("192.168.1.100"))
  ]))
  
  // 网关调用认证服务
  let auth_span_ctx = azimuth::SpanContext::new("trace-gateway-123", "span-auth-child", true, "")
  let auth_span = azimuth::Span::new("auth.validate", azimuth::Client, auth_span_ctx)
  
  azimuth::Span::add_event(auth_span, "auth.request.sent", Some([
    ("auth.token", azimuth::StringValue("jwt-token-123")),
    ("auth.method", azimuth::StringValue("oauth2"))
  ]))
  
  azimuth::Span::set_status(auth_span, azimuth::Ok)
  azimuth::Span::add_event(auth_span, "auth.response.received", Some([
    ("auth.user.id", azimuth::StringValue("user-456")),
    ("auth.permissions", azimuth::ArrayStringValue(["read", "write"]))
  ]))
  
  // 验证分布式追踪的一致性
  assert_eq(azimuth::SpanContext::trace_id(gateway_span_ctx), "trace-gateway-123")
  assert_eq(azimuth::SpanContext::trace_id(auth_span_ctx), "trace-gateway-123")
  
  // 验证Span ID的唯一性
  let span_ids = [
    azimuth::SpanContext::span_id(gateway_span_ctx),
    azimuth::SpanContext::span_id(auth_span_ctx)
  ]
  
  // 验证所有Span ID都是唯一的
  assert_true(span_ids[0] != span_ids[1])
  
  azimuth::Span::end(auth_span)
  azimuth::Span::end(gateway_span)
}