// Advanced MoonBit Test Cases for Azimuth Telemetry System
// This file contains new test cases focusing on edge cases and specialized functionality

// Test 1: Advanced Span event handling with complex attributes
pub test "高级Span事件处理复杂属性测试" {
  let span_ctx = azimuth::SpanContext::new("trace-adv-001", "span-adv-001", true, "key1=value1")
  let span = azimuth::Span::new("advanced-operation", azimuth::Client, span_ctx)
  
  // 创建复杂的事件属性
  let event_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(event_attrs, "event.type", azimuth::StringValue("custom"))
  azimuth::Attributes::set(event_attrs, "retry.count", azimuth::IntValue(3))
  azimuth::Attributes::set(event_attrs, "latency.ms", azimuth::FloatValue(125.5))
  azimuth::Attributes::set(event_attrs, "success", azimuth::BoolValue(true))
  azimuth::Attributes::set(event_attrs, "endpoints", azimuth::ArrayStringValue(["api1", "api2", "api3"]))
  
  // 添加多个事件
  azimuth::Span::add_event(span, "operation.started", Some(event_attrs))
  azimuth::Span::add_event(span, "retry.attempted", Some([("attempt", azimuth::IntValue(1))]))
  azimuth::Span::add_event(span, "retry.completed", Some([("attempt", azimuth::IntValue(3))]))
  azimuth::Span::add_event(span, "operation.completed", Some([("duration", azimuth::FloatValue(500.0))]))
  
  // 验证Span状态
  assert_eq(azimuth::Span::name(span), "advanced-operation")
  assert_eq(azimuth::Span::kind(span), azimuth::Client)
  assert_true(azimuth::Span::is_recording(span))
  
  // 设置Span状态并结束
  azimuth::Span::set_status(span, azimuth::Ok)
  azimuth::Span::end(span)
}

// Test 2: Metrics aggregation with complex time windows
pub test "度量聚合复杂时间窗口测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "aggregation-test")
  
  // 创建多个度量仪器
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total")
  let response_histogram = azimuth::Meter::create_histogram(meter, "http.response.duration")
  let error_gauge = azimuth::Meter::create_gauge(meter, "http.errors.active")
  
  // 模拟时间窗口内的度量记录
  let time_windows = [100, 200, 300, 400, 500]  // 模拟不同的响应时间
  
  for response_time in time_windows {
    azimuth::Counter::add(request_counter, 1.0)
    azimuth::Histogram::record(response_histogram, response_time.to_double())
  }
  
  // 模拟错误计数
  azimuth::Counter::add(request_counter, 5.0)  // 5个成功请求
  azimuth::Counter::add(request_counter, 2.0)  // 2个错误请求
  
  // 验证度量创建
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_histogram.name, "http.response.duration")
  assert_eq(error_gauge.name, "http.errors.active")
}

// Test 3: Context propagation with complex baggage scenarios
pub test "复杂场景上下文传播Baggage测试" {
  let root_ctx = azimuth::Context::root()
  
  // 创建复杂的Baggage场景
  let baggage = azimuth::Baggage::new()
  let baggage1 = azimuth::Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage2 = azimuth::Baggage::set_entry(baggage1, "request.id", "req-67890")
  let baggage3 = azimuth::Baggage::set_entry(baggage2, "session.id", "sess-abcde")
  let baggage4 = azimuth::Baggage::set_entry(baggage3, "trace.id", "trace-xyz")
  
  // 验证所有Baggage项
  assert_eq(azimuth::Baggage::get_entry(baggage4, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage4, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage4, "session.id"), Some("sess-abcde"))
  assert_eq(azimuth::Baggage::get_entry(baggage4, "trace.id"), Some("trace-xyz"))
  
  // 测试Baggage删除（如果支持）
  let baggage5 = azimuth::Baggage::set_entry(baggage4, "temp.value", "temp")
  assert_eq(azimuth::Baggage::get_entry(baggage5, "temp.value"), Some("temp"))
  
  // 测试长键名和值
  let long_key = "this.is.a.very.long.baggage.key.name.that.tests.system.robustness"
  let long_value = "this.is.a.very.long.baggage.value.that.tests.system.robustness.with.various.characters.123456789"
  let baggage6 = azimuth::Baggage::set_entry(baggage5, long_key, long_value)
  
  // 验证长键值对
  assert_eq(azimuth::Baggage::get_entry(baggage6, long_key), Some(long_value))
}

// Test 4: Advanced logging with structured data
pub test "高级日志结构化数据测试" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "structured-logger")
  
  // 创建结构化日志属性
  let log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(log_attrs, "log.level", azimuth::StringValue("INFO"))
  azimuth::Attributes::set(log_attrs, "component", azimuth::StringValue("authentication"))
  azimuth::Attributes::set(log_attrs, "user.id", azimuth::StringValue("user-123"))
  azimuth::Attributes::set(log_attrs, "ip.address", azimuth::StringValue("192.168.1.100"))
  azimuth::Attributes::set(log_attrs, "request.duration", azimuth::FloatValue(250.75))
  azimuth::Attributes::set(log_attrs, "success", azimuth::BoolValue(true))
  
  // 创建不同严重级别的日志记录
  let info_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("User authentication successful"),
    Some(log_attrs),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace-123"),
    Some("span-456"),
    Some(azimuth::Context::root())
  )
  
  let warn_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Authentication token will expire soon"),
    Some([("token.expires.in", azimuth::IntValue(300))]),
    Some(1735689600000001000L),
    None,
    Some("trace-123"),
    Some("span-456"),
    Some(azimuth::Context::root())
  )
  
  let error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Authentication failed for user"),
    Some([("user.id", azimuth::StringValue("user-456")), ("failure.reason", azimuth::StringValue("invalid.password"))]),
    Some(1735689600000002000L),
    None,
    Some("trace-789"),
    Some("span-012"),
    Some(azimuth::Context::root())
  )
  
  // 发出日志记录
  azimuth::Logger::emit(logger, info_log)
  azimuth::Logger::emit(logger, warn_log)
  azimuth::Logger::emit(logger, error_log)
  
  // 验证日志记录属性
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  
  assert_eq(azimuth::LogRecord::body(info_log), Some("User authentication successful"))
  assert_eq(azimuth::LogRecord::body(warn_log), Some("Authentication token will expire soon"))
  assert_eq(azimuth::LogRecord::body(error_log), Some("Authentication failed for user"))
}

// Test 5: Resource with complex hierarchical attributes
pub test "资源复杂层次属性测试" {
  let base_resource = azimuth::Resource::new()
  
  // 添加基础服务属性
  let service_attrs = [
    ("service.name", azimuth::StringValue("advanced-service")),
    ("service.version", azimuth::StringValue("2.1.0")),
    ("service.instance.id", azimuth::StringValue("instance-adv-001")),
    ("service.namespace", azimuth::StringValue("production"))
  ]
  
  let service_resource = azimuth::Resource::with_attributes(base_resource, service_attrs)
  
  // 添加主机属性
  let host_attrs = [
    ("host.name", azimuth::StringValue("host-adv-001")),
    ("host.type", azimuth::StringValue("virtual")),
    ("host.arch", azimuth::StringValue("x86_64")),
    ("host.os.family", azimuth::StringValue("linux")),
    ("host.os.version", azimuth::StringValue("5.15.0"))
  ]
  
  let host_resource = azimuth::Resource::with_attributes(service_resource, host_attrs)
  
  // 添加部署属性
  let deployment_attrs = [
    ("deployment.environment", azimuth::StringValue("production")),
    ("deployment.region", azimuth::StringValue("us-west-2")),
    ("deployment.zone", azimuth::StringValue("us-west-2a")),
    ("deployment.cluster", azimuth::StringValue("main-cluster"))
  ]
  
  let full_resource = azimuth::Resource::with_attributes(host_resource, deployment_attrs)
  
  // 验证所有层次的属性
  assert_eq(azimuth::Resource::get_attribute(full_resource, "service.name"), Some(azimuth::StringValue("advanced-service")))
  assert_eq(azimuth::Resource::get_attribute(full_resource, "service.version"), Some(azimuth::StringValue("2.1.0")))
  assert_eq(azimuth::Resource::get_attribute(full_resource, "host.name"), Some(azimuth::StringValue("host-adv-001")))
  assert_eq(azimuth::Resource::get_attribute(full_resource, "deployment.environment"), Some(azimuth::StringValue("production")))
  
  // 测试资源合并策略
  let override_attrs = [
    ("service.version", azimuth::StringValue("2.2.0")),  // 应该覆盖原有值
    ("new.feature", azimuth::StringValue("enabled"))     // 新增属性
  ]
  
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), override_attrs)
  let merged_resource = azimuth::Resource::merge(full_resource, override_resource)
  
  // 验证合并结果
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), Some(azimuth::StringValue("advanced-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), Some(azimuth::StringValue("2.2.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "new.feature"), Some(azimuth::StringValue("enabled")))
}

// Test 6: Multi-format propagator compatibility
pub test "多格式传播器兼容性测试" {
  // 创建不同类型的传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let custom_propagator = azimuth::CustomPropagator::new("custom-format")
  
  // 创建复合传播器
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 创建载体和上下文
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  
  // 添加上下文值
  let key1 = azimuth::ContextKey::new("user.id")
  let key2 = azimuth::ContextKey::new("request.id")
  let ctx_with_values = azimuth::Context::with_value(
    azimuth::Context::with_value(ctx, key1, "user-123"),
    key2, "req-456"
  )
  
  // 注入上下文
  azimuth::CompositePropagator::inject(composite_propagator, ctx_with_values, carrier)
  
  // 验证注入的头部
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  let baggage_header = azimuth::TextMapCarrier::get(carrier, "baggage")
  
  // 验证头部存在（具体值取决于实现）
  assert_true(trace_header != None)
  assert_true(baggage_header != None)
  
  // 提取上下文
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  let extracted_value1 = azimuth::Context::get(extracted_ctx, key1)
  let extracted_value2 = azimuth::Context::get(extracted_ctx, key2)
  
  // 验证提取的值
  assert_eq(extracted_value1, Some("user-123"))
  assert_eq(extracted_value2, Some("req-456"))
}

// Test 7: Advanced sampling strategies
pub test "高级采样策略测试" {
  // 创建不同的采样器
  let always_on_sampler = azimuth::AlwaysOnSampler::new()
  let always_off_sampler = azimuth::AlwaysOffSampler::new()
  let trace_id_ratio_sampler = azimuth::TraceIdRatioBasedSampler::new(0.5)  // 50%采样率
  
  // 测试不同的Span上下文
  let span_ctx1 = azimuth::SpanContext::new("trace-sampler-001", "span-001", true, "")
  let span_ctx2 = azimuth::SpanContext::new("trace-sampler-002", "span-002", false, "")
  let span_ctx3 = azimuth::SpanContext::new("trace-sampler-003", "span-003", true, "")
  
  // 测试AlwaysOn采样器
  let decision1 = azimuth::Sampler::should_sample(always_on_sampler, span_ctx1, "test-operation")
  assert_eq(decision1, azimuth::RecordAndSample)
  
  // 测试AlwaysOff采样器
  let decision2 = azimuth::Sampler::should_sample(always_off_sampler, span_ctx2, "test-operation")
  assert_eq(decision2, azimuth::Drop)
  
  // 测试TraceIdRatioBased采样器
  let decision3 = azimuth::Sampler::should_sample(trace_id_ratio_sampler, span_ctx3, "test-operation")
  // 结果取决于trace-id和采样率的计算，这里只测试方法调用
  
  // 创建带有采样决策的Span
  if decision3 == azimuth::RecordAndSample {
    let sampled_span = azimuth::Span::new("sampled-operation", azimuth::Server, span_ctx3)
    assert_true(azimuth::Span::is_recording(sampled_span))
  }
}

// Test 8: Performance metrics under load
pub test "负载下性能指标测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "load-test-meter")
  
  // 创建多个度量仪器
  let throughput_counter = azimuth::Meter::create_counter(meter, "operations.throughput")
  let latency_histogram = azimuth::Meter::create_histogram(meter, "operations.latency")
  let error_counter = azimuth::Meter::create_counter(meter, "operations.errors")
  let active_gauge = azimuth::Meter::create_gauge(meter, "operations.active")
  
  // 模拟高负载场景
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量操作
  let batch_sizes = [10, 50, 100, 200, 500]
  
  for batch_size in batch_sizes {
    let batch_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    // 模拟批量操作
    for i in 0..batch_size {
      azimuth::Counter::add(throughput_counter, 1.0)
      
      // 模拟不同的延迟值
      let latency = 50.0 + (i.to_double() % 200.0)  // 50-250ms范围
      azimuth::Histogram::record(latency_histogram, latency)
      
      // 模拟偶发错误（约5%错误率）
      if i % 20 == 0 {
        azimuth::Counter::add(error_counter, 1.0)
      }
    }
    
    let batch_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let batch_duration = batch_end - batch_start
    
    // 记录批量处理时间
    azimuth::Histogram::record(latency_histogram, batch_duration.to_double() / 1000000.0)  // 转换为毫秒
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let total_duration = end_time - start_time
  
  // 验证性能在合理范围内
  assert_true(total_duration < 10000000000L)  // 小于10秒
  
  // 验证度量创建
  assert_eq(throughput_counter.name, "operations.throughput")
  assert_eq(latency_histogram.name, "operations.latency")
  assert_eq(error_counter.name, "operations.errors")
  assert_eq(active_gauge.name, "operations.active")
}

// Test 9: Cross-service distributed tracing
pub test "跨服务分布式追踪测试" {
  // 创建多个服务的Tracer
  let api_gateway_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "api-gateway", 
    Some("1.5.0")
  )
  
  let auth_service_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "auth-service", 
    Some("2.1.0")
  )
  
  let user_service_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "user-service", 
    Some("1.8.0")
  )
  
  // 创建根Span（API网关）
  let root_span_ctx = azimuth::SpanContext::new("dist-trace-001", "gateway-root", true, "")
  let gateway_span = azimuth::Span::new("api.request", azimuth::Server, root_span_ctx)
  
  // API网关调用认证服务
  let auth_span_ctx = azimuth::SpanContext::new("dist-trace-001", "auth-child", true, "")
  let auth_span = azimuth::Span::new("authenticate.user", azimuth::Client, auth_span_ctx)
  
  // 认证服务调用用户服务
  let user_span_ctx = azimuth::SpanContext::new("dist-trace-001", "user-grandchild", true, "")
  let user_span = azimuth::Span::new("get.user.profile", azimuth::Client, user_span_ctx)
  
  // 验证Trace ID一致性
  assert_eq(azimuth::SpanContext::trace_id(root_span_ctx), "dist-trace-001")
  assert_eq(azimuth::SpanContext::trace_id(auth_span_ctx), "dist-trace-001")
  assert_eq(azimuth::SpanContext::trace_id(user_span_ctx), "dist-trace-001")
  
  // 验证Span ID唯一性
  assert_true(azimuth::SpanContext::span_id(root_span_ctx) != azimuth::SpanContext::span_id(auth_span_ctx))
  assert_true(azimuth::SpanContext::span_id(auth_span_ctx) != azimuth::SpanContext::span_id(user_span_ctx))
  
  // 验证Span层次关系
  assert_eq(azimuth::Span::name(gateway_span), "api.request")
  assert_eq(azimuth::Span::name(auth_span), "authenticate.user")
  assert_eq(azimuth::Span::name(user_span), "get.user.profile")
  
  // 添加事件和属性
  azimuth::Span::add_event(gateway_span, "request.received", Some([("endpoint", azimuth::StringValue("/api/v1/users"))]))
  azimuth::Span::add_event(auth_span, "authentication.started", Some([("user.id", azimuth::StringValue("user-123"))]))
  azimuth::Span::add_event(user_span, "profile.fetch.started", None)
  
  // 设置状态并结束Span
  azimuth::Span::set_status(user_span, azimuth::Ok)
  azimuth::Span::end(user_span)
  
  azimuth::Span::set_status(auth_span, azimuth::Ok)
  azimuth::Span::end(auth_span)
  
  azimuth::Span::set_status(gateway_span, azimuth::Ok)
  azimuth::Span::end(gateway_span)
}

// Test 10: Advanced error handling and recovery
pub test "高级错误处理和恢复测试" {
  // 创建测试上下文
  let ctx = azimuth::Context::root()
  
  // 测试Span错误处理
  let error_span_ctx = azimuth::SpanContext::new("error-trace-001", "error-span", true, "")
  let error_span = azimuth::Span::new("error.prone.operation", azimuth::Internal, error_span_ctx)
  
  // 添加错误事件
  let error_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(error_attrs, "error.type", azimuth::StringValue("TimeoutError"))
  azimuth::Attributes::set(error_attrs, "error.message", azimuth::StringValue("Operation timed out after 30 seconds"))
  azimuth::Attributes::set(error_attrs, "retry.count", azimuth::IntValue(3))
  azimuth::Attributes::set(error_attrs, "error.code", azimuth::IntValue(504))
  
  azimuth::Span::add_event(error_span, "error.occurred", Some(error_attrs))
  azimuth::Span::set_status(error_span, azimuth::Error)
  azimuth::Span::end(error_span)
  
  // 测试日志错误处理
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "error-logger")
  
  let error_log_attrs = [
    ("error.type", azimuth::StringValue("ValidationError")),
    ("error.message", azimuth::StringValue("Invalid input parameter")),
    ("input.value", azimuth::StringValue("invalid-value")),
    ("expected.format", azimuth::StringValue("uuid"))
  ]
  
  let error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Validation failed for input parameter"),
    Some(error_log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("error-trace-001"),
    Some("error-span"),
    Some(ctx)
  )
  
  azimuth::Logger::emit(logger, error_log)
  
  // 测试度量错误处理
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "error-metrics")
  let error_counter = azimuth::Meter::create_counter(meter, "errors.total")
  let error_gauge = azimuth::Meter::create_gauge(meter, "errors.active")
  
  // 记录不同类型的错误
  azimuth::Counter::add(error_counter, 1.0)  // 通用错误计数
  
  // 测试资源错误处理
  let error_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("error.state", azimuth::StringValue("recovery")),
    ("last.error.time", azimuth::StringValue("2025-01-01T12:00:00Z")),
    ("error.count", azimuth::IntValue(1)),
    ("recovery.attempts", azimuth::IntValue(3))
  ])
  
  // 验证错误状态
  assert_eq(azimuth::Resource::get_attribute(error_resource, "error.state"), Some(azimuth::StringValue("recovery")))
  assert_eq(azimuth::Resource::get_attribute(error_resource, "error.count"), Some(azimuth::IntValue(1)))
  
  // 测试恢复场景
  let recovery_span_ctx = azimuth::SpanContext::new("recovery-trace-001", "recovery-span", true, "")
  let recovery_span = azimuth::Span::new("recovery.operation", azimuth::Internal, recovery_span_ctx)
  
  azimuth::Span::add_event(recovery_span, "recovery.started", Some([("attempt", azimuth::IntValue(1))]))
  azimuth::Span::set_status(recovery_span, azimuth::Ok)
  azimuth::Span::end(recovery_span)
  
  // 验证恢复状态
  let recovery_resource = azimuth::Resource::with_attributes(error_resource, [
    ("recovery.status", azimuth::StringValue("success")),
    ("recovery.time", azimuth::StringValue("2025-01-01T12:05:00Z"))
  ])
  
  assert_eq(azimuth::Resource::get_attribute(recovery_resource, "recovery.status"), Some(azimuth::StringValue("success")))
}