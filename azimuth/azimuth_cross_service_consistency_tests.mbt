// Azimuth Cross-Service Consistency Tests
// This file contains tests for cross-service consistency in distributed telemetry scenarios

// Test 1: Distributed trace consistency across services
pub test "跨服务一致性测试：分布式追踪一致性" {
  // 模拟微服务架构中的分布式追踪
  let service_a_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-a")
  let service_b_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-b")
  let service_c_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-c")
  
  // 服务A创建根Span
  let root_trace_id = "trace-12345-abcde-67890"
  let root_span_id = "span-a-root-12345"
  let root_span_ctx = azimuth::SpanContext::new(root_trace_id, root_span_id, true, "key1=value1,key2=value2")
  let root_span = azimuth::Span::new("service-a-operation", azimuth::Server, root_span_ctx)
  
  // 服务A调用服务B
  let span_b_id = "span-b-child-23456"
  let span_b_ctx = azimuth::SpanContext::new(root_trace_id, span_b_id, true, "key1=value1,key2=value2,service=b")
  let span_b = azimuth::Span::new("service-b-operation", azimuth::Server, span_b_ctx)
  
  // 服务B调用服务C
  let span_c_id = "span-c-grandchild-34567"
  let span_c_ctx = azimuth::SpanContext::new(root_trace_id, span_c_id, true, "key1=value1,key2=value2,service=b,service=c")
  let span_c = azimuth::Span::new("service-c-operation", azimuth::Internal, span_c_ctx)
  
  // 服务C调用服务D（异步操作）
  let span_d_id = "span-d-async-45678"
  let span_d_ctx = azimuth::SpanContext::new(root_trace_id, span_d_id, true, "key1=value1,key2=value2,service=b,service=c,service=d")
  let span_d = azimuth::Span::new("service-d-async-operation", azimuth::Client, span_d_ctx)
  
  // 验证Trace ID一致性
  assert_eq(azimuth::SpanContext::trace_id(root_span_ctx), root_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(span_b_ctx), root_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(span_c_ctx), root_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(span_d_ctx), root_trace_id)
  
  // 验证Span ID唯一性
  assert_true(azimuth::SpanContext::span_id(root_span_ctx) != azimuth::SpanContext::span_id(span_b_ctx))
  assert_true(azimuth::SpanContext::span_id(span_b_ctx) != azimuth::SpanContext::span_id(span_c_ctx))
  assert_true(azimuth::SpanContext::span_id(span_c_ctx) != azimuth::SpanContext::span_id(span_d_ctx))
  assert_true(azimuth::SpanContext::span_id(root_span_ctx) != azimuth::SpanContext::span_id(span_d_ctx))
  
  // 验证Span Kind正确性
  assert_eq(azimuth::Span::kind(root_span), azimuth::Server)
  assert_eq(azimuth::Span::kind(span_b), azimuth::Server)
  assert_eq(azimuth::Span::kind(span_c), azimuth::Internal)
  assert_eq(azimuth::Span::kind(span_d), azimuth::Client)
  
  // 验证采样标志一致性
  assert_true(azimuth::SpanContext::is_sampled(root_span_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_b_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_c_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_d_ctx))
  
  // 验证Trace State传播和累积
  assert_eq(azimuth::SpanContext::trace_state(root_span_ctx), "key1=value1,key2=value2")
  assert_eq(azimuth::SpanContext::trace_state(span_b_ctx), "key1=value1,key2=value2,service=b")
  assert_eq(azimuth::SpanContext::trace_state(span_c_ctx), "key1=value1,key2=value2,service=b,service=c")
  assert_eq(azimuth::SpanContext::trace_state(span_d_ctx), "key1=value1,key2=value2,service=b,service=c,service=d")
}

// Test 2: Cross-service baggage propagation consistency
pub test "跨服务一致性测试：Baggage传播一致性" {
  // 模拟跨服务的Baggage传播
  let baggage_a = azimuth::Baggage::new()
  
  // 服务A添加初始Baggage
  baggage_a = azimuth::Baggage::set_entry(baggage_a, "user.id", "12345")
  baggage_a = azimuth::Baggage::set_entry(baggage_a, "request.id", "req-67890")
  baggage_a = azimuth::Baggage::set_entry(baggage_a, "session.id", "session-abc123")
  
  // 服务B添加更多Baggage
  let baggage_b = baggage_a
  baggage_b = azimuth::Baggage::set_entry(baggage_b, "service.b.timestamp", "2025-01-01T12:00:00Z")
  baggage_b = azimuth::Baggage::set_entry(baggage_b, "service.b.operation", "process-data")
  
  // 服务C添加更多Baggage
  let baggage_c = baggage_b
  baggage_c = azimuth::Baggage::set_entry(baggage_c, "service.c.result", "success")
  baggage_c = azimuth::Baggage::set_entry(baggage_c, "service.c.duration", "150ms")
  
  // 服务D修改现有Baggage
  let baggage_d = baggage_c
  baggage_d = azimuth::Baggage::set_entry(baggage_d, "service.c.duration", "180ms")  // 覆盖之前的值
  baggage_d = azimuth::Baggage::set_entry(baggage_d, "service.d.status", "completed")
  
  // 验证Baggage传播的完整性
  assert_eq(azimuth::Baggage::get_entry(baggage_a, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_a, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage_a, "session.id"), Some("session-abc123"))
  assert_eq(azimuth::Baggage::get_entry(baggage_a, "service.b.timestamp"), None)
  assert_eq(azimuth::Baggage::get_entry(baggage_a, "service.c.result"), None)
  
  assert_eq(azimuth::Baggage::get_entry(baggage_b, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_b, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage_b, "session.id"), Some("session-abc123"))
  assert_eq(azimuth::Baggage::get_entry(baggage_b, "service.b.timestamp"), Some("2025-01-01T12:00:00Z"))
  assert_eq(azimuth::Baggage::get_entry(baggage_b, "service.b.operation"), Some("process-data"))
  
  assert_eq(azimuth::Baggage::get_entry(baggage_c, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c, "session.id"), Some("session-abc123"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c, "service.b.timestamp"), Some("2025-01-01T12:00:00Z"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c, "service.b.operation"), Some("process-data"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c, "service.c.result"), Some("success"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c, "service.c.duration"), Some("150ms"))
  
  assert_eq(azimuth::Baggage::get_entry(baggage_d, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_d, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage_d, "session.id"), Some("session-abc123"))
  assert_eq(azimuth::Baggage::get_entry(baggage_d, "service.b.timestamp"), Some("2025-01-01T12:00:00Z"))
  assert_eq(azimuth::Baggage::get_entry(baggage_d, "service.b.operation"), Some("process-data"))
  assert_eq(azimuth::Baggage::get_entry(baggage_d, "service.c.result"), Some("success"))
  assert_eq(azimuth::Baggage::get_entry(baggage_d, "service.c.duration"), Some("180ms"))  // 更新后的值
  assert_eq(azimuth::Baggage::get_entry(baggage_d, "service.d.status"), Some("completed"))
}

// Test 3: Cross-service metrics consistency
pub test "跨服务一致性测试：度量一致性" {
  // 模拟跨服务的度量收集
  let meter_a = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "service-a")
  let meter_b = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "service-b")
  let meter_c = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "service-c")
  
  // 服务A创建度量
  let request_counter_a = azimuth::Meter::create_counter(meter_a, "http.requests.total", Some("Total HTTP requests"), Some("count"))
  let response_time_a = azimuth::Meter::create_histogram(meter_a, "http.response.time", Some("HTTP response time"), Some("ms"))
  let active_connections_a = azimuth::Meter::create_updown_counter(meter_a, "http.active.connections", Some("Active HTTP connections"), Some("connections"))
  
  // 服务B创建相同名称的度量（测试一致性）
  let request_counter_b = azimuth::Meter::create_counter(meter_b, "http.requests.total", Some("Total HTTP requests"), Some("count"))
  let response_time_b = azimuth::Meter::create_histogram(meter_b, "http.response.time", Some("HTTP response time"), Some("ms"))
  let error_rate_b = azimuth::Meter::create_counter(meter_b, "http.errors.total", Some("Total HTTP errors"), Some("count"))
  
  // 服务C创建相同名称的度量
  let request_counter_c = azimuth::Meter::create_counter(meter_c, "http.requests.total", Some("Total HTTP requests"), Some("count"))
  let response_time_c = azimuth::Meter::create_histogram(meter_c, "http.response.time", Some("HTTP response time"), Some("ms"))
  let throughput_c = azimuth::Meter::create_counter(meter_c, "http.throughput", Some("HTTP throughput"), Some("bytes/sec"))
  
  // 验证度量名称和描述的一致性
  assert_eq(request_counter_a.name, "http.requests.total")
  assert_eq(request_counter_b.name, "http.requests.total")
  assert_eq(request_counter_c.name, "http.requests.total")
  
  assert_eq(request_counter_a.description, Some("Total HTTP requests"))
  assert_eq(request_counter_b.description, Some("Total HTTP requests"))
  assert_eq(request_counter_c.description, Some("Total HTTP requests"))
  
  assert_eq(request_counter_a.unit, Some("count"))
  assert_eq(request_counter_b.unit, Some("count"))
  assert_eq(request_counter_c.unit, Some("count"))
  
  assert_eq(response_time_a.name, "http.response.time")
  assert_eq(response_time_b.name, "http.response.time")
  assert_eq(response_time_c.name, "http.response.time")
  
  assert_eq(response_time_a.description, Some("HTTP response time"))
  assert_eq(response_time_b.description, Some("HTTP response time"))
  assert_eq(response_time_c.description, Some("HTTP response time"))
  
  assert_eq(response_time_a.unit, Some("ms"))
  assert_eq(response_time_b.unit, Some("ms"))
  assert_eq(response_time_c.unit, Some("ms"))
  
  // 记录度量值
  azimuth::Counter::add(request_counter_a, 100.0)
  azimuth::Histogram::record(response_time_a, 150.5)
  azimuth::UpDownCounter::add(active_connections_a, 25.0)
  
  azimuth::Counter::add(request_counter_b, 80.0)
  azimuth::Histogram::record(response_time_b, 200.3)
  azimuth::Counter::add(error_rate_b, 5.0)
  
  azimuth::Counter::add(request_counter_c, 120.0)
  azimuth::Histogram::record(response_time_c, 120.7)
  azimuth::Counter::add(throughput_c, 1024.0)
  
  // 验证度量操作的完整性
  assert_eq(request_counter_a.name, "http.requests.total")
  assert_eq(request_counter_b.name, "http.requests.total")
  assert_eq(request_counter_c.name, "http.requests.total")
  
  assert_eq(response_time_a.name, "http.response.time")
  assert_eq(response_time_b.name, "http.response.time")
  assert_eq(response_time_c.name, "http.response.time")
}

// Test 4: Cross-service logging consistency
pub test "跨服务一致性测试：日志一致性" {
  // 模拟跨服务的日志记录
  let logger_a = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "service-a")
  let logger_b = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "service-b")
  let logger_c = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "service-c")
  
  // 创建关联的日志记录
  let trace_id = "trace-12345-abcde-67890"
  let base_timestamp = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 服务A记录日志
  let log_a = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service A: Processing request"),
    Some(azimuth::Attributes::new()),
    Some(base_timestamp),
    Some(base_timestamp + 1000000L),
    Some(trace_id),
    Some("span-a-12345"),
    Some(azimuth::Context::root())
  )
  
  // 服务B记录日志
  let log_b = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service B: Handling sub-request"),
    Some(azimuth::Attributes::new()),
    Some(base_timestamp + 5000000L),
    Some(base_timestamp + 6000000L),
    Some(trace_id),
    Some("span-b-23456"),
    Some(azimuth::Context::root())
  )
  
  // 服务C记录日志
  let log_c = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Service C: Warning condition detected"),
    Some(azimuth::Attributes::new()),
    Some(base_timestamp + 10000000L),
    Some(base_timestamp + 11000000L),
    Some(trace_id),
    Some("span-c-34567"),
    Some(azimuth::Context::root())
  )
  
  // 验证日志记录的关联性
  assert_eq(azimuth::LogRecord::trace_id(log_a), Some(trace_id))
  assert_eq(azimuth::LogRecord::trace_id(log_b), Some(trace_id))
  assert_eq(azimuth::LogRecord::trace_id(log_c), Some(trace_id))
  
  assert_eq(azimuth::LogRecord::span_id(log_a), Some("span-a-12345"))
  assert_eq(azimuth::LogRecord::span_id(log_b), Some("span-b-23456"))
  assert_eq(azimuth::LogRecord::span_id(log_c), Some("span-c-34567"))
  
  // 验证日志严重级别
  assert_eq(azimuth::LogRecord::severity_number(log_a), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(log_b), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(log_c), azimuth::Warn)
  
  // 验证日志消息
  assert_eq(azimuth::LogRecord::body(log_a), Some("Service A: Processing request"))
  assert_eq(azimuth::LogRecord::body(log_b), Some("Service B: Handling sub-request"))
  assert_eq(azimuth::LogRecord::body(log_c), Some("Service C: Warning condition detected"))
  
  // 验证时间戳顺序
  assert_true(azimuth::LogRecord::timestamp(log_a).unwrap() < azimuth::LogRecord::timestamp(log_b).unwrap())
  assert_true(azimuth::LogRecord::timestamp(log_b).unwrap() < azimuth::LogRecord::timestamp(log_c).unwrap())
  
  // 发出日志
  azimuth::Logger::emit(logger_a, log_a)
  azimuth::Logger::emit(logger_b, log_b)
  azimuth::Logger::emit(logger_c, log_c)
  
  // 验证Logger配置
  assert_eq(logger_a.scope.name, "service-a")
  assert_eq(logger_b.scope.name, "service-b")
  assert_eq(logger_c.scope.name, "service-c")
}

// Test 5: Cross-service resource consistency
pub test "跨服务一致性测试：资源一致性" {
  // 模拟跨服务的资源配置
  let base_attrs = [
    ("service.namespace", azimuth::StringValue("production")),
    ("deployment.environment", azimuth::StringValue("prod")),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("1.0.0")),
    ("telemetry.sdk.language", azimuth::StringValue("moonbit"))
  ]
  
  // 服务A的资源配置
  let service_a_attrs = [
    ("service.name", azimuth::StringValue("user-service")),
    ("service.version", azimuth::StringValue("2.1.0")),
    ("service.instance.id", azimuth::StringValue("user-service-123")),
    ("host.name", azimuth::StringValue("host-a-001"))
  ]
  
  // 服务B的资源配置
  let service_b_attrs = [
    ("service.name", azimuth::StringValue("order-service")),
    ("service.version", azimuth::StringValue("1.5.2")),
    ("service.instance.id", azimuth::StringValue("order-service-456")),
    ("host.name", azimuth::StringValue("host-b-002"))
  ]
  
  // 服务C的资源配置
  let service_c_attrs = [
    ("service.name", azimuth::StringValue("payment-service")),
    ("service.version", azimuth::StringValue("3.0.1")),
    ("service.instance.id", azimuth::StringValue("payment-service-789")),
    ("host.name", azimuth::StringValue("host-c-003"))
  ]
  
  // 创建资源
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), base_attrs)
  let resource_a = azimuth::Resource::with_attributes(base_resource, service_a_attrs)
  let resource_b = azimuth::Resource::with_attributes(base_resource, service_b_attrs)
  let resource_c = azimuth::Resource::with_attributes(base_resource, service_c_attrs)
  
  // 验证基础资源属性的一致性
  assert_eq(azimuth::Resource::get_attribute(resource_a, "service.namespace"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(resource_b, "service.namespace"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(resource_c, "service.namespace"), Some(azimuth::StringValue("production")))
  
  assert_eq(azimuth::Resource::get_attribute(resource_a, "deployment.environment"), Some(azimuth::StringValue("prod")))
  assert_eq(azimuth::Resource::get_attribute(resource_b, "deployment.environment"), Some(azimuth::StringValue("prod")))
  assert_eq(azimuth::Resource::get_attribute(resource_c, "deployment.environment"), Some(azimuth::StringValue("prod")))
  
  assert_eq(azimuth::Resource::get_attribute(resource_a, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(resource_b, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(resource_c, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  
  // 验证服务特定属性的唯一性
  assert_eq(azimuth::Resource::get_attribute(resource_a, "service.name"), Some(azimuth::StringValue("user-service")))
  assert_eq(azimuth::Resource::get_attribute(resource_b, "service.name"), Some(azimuth::StringValue("order-service")))
  assert_eq(azimuth::Resource::get_attribute(resource_c, "service.name"), Some(azimuth::StringValue("payment-service")))
  
  assert_eq(azimuth::Resource::get_attribute(resource_a, "service.version"), Some(azimuth::StringValue("2.1.0")))
  assert_eq(azimuth::Resource::get_attribute(resource_b, "service.version"), Some(azimuth::StringValue("1.5.2")))
  assert_eq(azimuth::Resource::get_attribute(resource_c, "service.version"), Some(azimuth::StringValue("3.0.1")))
  
  assert_eq(azimuth::Resource::get_attribute(resource_a, "service.instance.id"), Some(azimuth::StringValue("user-service-123")))
  assert_eq(azimuth::Resource::get_attribute(resource_b, "service.instance.id"), Some(azimuth::StringValue("order-service-456")))
  assert_eq(azimuth::Resource::get_attribute(resource_c, "service.instance.id"), Some(azimuth::StringValue("payment-service-789")))
}

// Test 6: Cross-service context propagation consistency
pub test "跨服务一致性测试：上下文传播一致性" {
  // 模拟跨服务的上下文传播
  let root_ctx = azimuth::Context::root()
  
  // 服务A添加上下文
  let user_key = azimuth::ContextKey::new("user.id")
  let request_key = azimuth::ContextKey::new("request.id")
  let session_key = azimuth::ContextKey::new("session.id")
  
  let ctx_a = azimuth::Context::with_value(root_ctx, user_key, "user-12345")
  let ctx_a = azimuth::Context::with_value(ctx_a, request_key, "req-abcdef")
  let ctx_a = azimuth::Context::with_value(ctx_a, session_key, "session-98765")
  
  // 服务B添加上下文
  let service_b_key = azimuth::ContextKey::new("service.b.operation")
  let timestamp_b_key = azimuth::ContextKey::new("service.b.timestamp")
  
  let ctx_b = azimuth::Context::with_value(ctx_a, service_b_key, "process-order")
  let ctx_b = azimuth::Context::with_value(ctx_b, timestamp_b_key, "2025-01-01T12:00:00Z")
  
  // 服务C添加上下文
  let service_c_key = azimuth::ContextKey::new("service.c.result")
  let duration_c_key = azimuth::ContextKey::new("service.c.duration")
  
  let ctx_c = azimuth::Context::with_value(ctx_b, service_c_key, "success")
  let ctx_c = azimuth::Context::with_value(ctx_c, duration_c_key, "150ms")
  
  // 验证上下文传播的完整性
  assert_eq(azimuth::Context::get(ctx_a, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(ctx_a, request_key), Some("req-abcdef"))
  assert_eq(azimuth::Context::get(ctx_a, session_key), Some("session-98765"))
  assert_eq(azimuth::Context::get(ctx_a, service_b_key), None)
  assert_eq(azimuth::Context::get(ctx_a, service_c_key), None)
  
  assert_eq(azimuth::Context::get(ctx_b, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(ctx_b, request_key), Some("req-abcdef"))
  assert_eq(azimuth::Context::get(ctx_b, session_key), Some("session-98765"))
  assert_eq(azimuth::Context::get(ctx_b, service_b_key), Some("process-order"))
  assert_eq(azimuth::Context::get(ctx_b, timestamp_b_key), Some("2025-01-01T12:00:00Z"))
  assert_eq(azimuth::Context::get(ctx_b, service_c_key), None)
  
  assert_eq(azimuth::Context::get(ctx_c, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(ctx_c, request_key), Some("req-abcdef"))
  assert_eq(azimuth::Context::get(ctx_c, session_key), Some("session-98765"))
  assert_eq(azimuth::Context::get(ctx_c, service_b_key), Some("process-order"))
  assert_eq(azimuth::Context::get(ctx_c, timestamp_b_key), Some("2025-01-01T12:00:00Z"))
  assert_eq(azimuth::Context::get(ctx_c, service_c_key), Some("success"))
  assert_eq(azimuth::Context::get(ctx_c, duration_c_key), Some("150ms"))
}

// Test 7: Cross-service instrumentation scope consistency
pub test "跨服务一致性测试：工具范围一致性" {
  // 模拟跨服务的工具范围配置
  let tracer_provider = azimuth::TracerProvider::default()
  let meter_provider = azimuth::MeterProvider::default()
  let logger_provider = azimuth::LoggerProvider::default()
  
  // 服务A的工具范围
  let tracer_a = azimuth::TracerProvider::get_tracer(tracer_provider, "user-service", Some("2.1.0"))
  let meter_a = azimuth::MeterProvider::get_meter(meter_provider, "user-service", Some("2.1.0"))
  let logger_a = azimuth::LoggerProvider::get_logger(logger_provider, "user-service")
  
  // 服务B的工具范围
  let tracer_b = azimuth::TracerProvider::get_tracer(tracer_provider, "order-service", Some("1.5.2"))
  let meter_b = azimuth::MeterProvider::get_meter(meter_provider, "order-service", Some("1.5.2"))
  let logger_b = azimuth::LoggerProvider::get_logger(logger_provider, "order-service")
  
  // 服务C的工具范围
  let tracer_c = azimuth::TracerProvider::get_tracer(tracer_provider, "payment-service", Some("3.0.1"))
  let meter_c = azimuth::MeterProvider::get_meter(meter_provider, "payment-service", Some("3.0.1"))
  let logger_c = azimuth::LoggerProvider::get_logger(logger_provider, "payment-service")
  
  // 验证工具范围的一致性
  let scope_tracer_a = azimuth::Tracer::instrumentation_scope(tracer_a)
  let scope_tracer_b = azimuth::Tracer::instrumentation_scope(tracer_b)
  let scope_tracer_c = azimuth::Tracer::instrumentation_scope(tracer_c)
  
  assert_eq(scope_tracer_a.name, "user-service")
  assert_eq(scope_tracer_a.version, Some("2.1.0"))
  
  assert_eq(scope_tracer_b.name, "order-service")
  assert_eq(scope_tracer_b.version, Some("1.5.2"))
  
  assert_eq(scope_tracer_c.name, "payment-service")
  assert_eq(scope_tracer_c.version, Some("3.0.1"))
  
  // 验证Logger的范围
  assert_eq(logger_a.scope.name, "user-service")
  assert_eq(logger_b.scope.name, "order-service")
  assert_eq(logger_c.scope.name, "payment-service")
  
  // 验证Meter的范围（通过创建的度量工具间接验证）
  let counter_a = azimuth::Meter::create_counter(meter_a, "test.counter")
  let counter_b = azimuth::Meter::create_counter(meter_b, "test.counter")
  let counter_c = azimuth::Meter::create_counter(meter_c, "test.counter")
  
  // 注意：这里简化实现可能不支持获取Meter的范围信息
  // 在实际实现中，应该验证Meter的范围与服务名称一致
}

// Test 8: Cross-service error handling consistency
pub test "跨服务一致性测试：错误处理一致性" {
  // 模拟跨服务的错误处理
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer_a = azimuth::TracerProvider::get_tracer(tracer_provider, "service-a")
  let tracer_b = azimuth::TracerProvider::get_tracer(tracer_provider, "service-b")
  let tracer_c = azimuth::TracerProvider::get_tracer(tracer_provider, "service-c")
  
  // 创建错误场景的Span
  let error_trace_id = "error-trace-12345"
  
  // 服务A遇到错误
  let span_a_ctx = azimuth::SpanContext::new(error_trace_id, "span-a-error", true, "")
  let span_a = azimuth::Span::new("service-a-operation", azimuth::Server, span_a_ctx)
  azimuth::Span::set_status(span_a, azimuth::Error)
  azimuth::Span::add_event(span_a, "Error occurred", Some([("error.message", azimuth::StringValue("Database connection failed"))]))
  
  // 服务B处理错误
  let span_b_ctx = azimuth::SpanContext::new(error_trace_id, "span-b-error", true, "")
  let span_b = azimuth::Span::new("service-b-operation", azimuth::Client, span_b_ctx)
  azimuth::Span::set_status(span_b, azimuth::Error)
  azimuth::Span::add_event(span_b, "Error propagated", Some([("error.code", azimuth::StringValue("DB_ERROR"))]))
  
  // 服务C恢复错误
  let span_c_ctx = azimuth::SpanContext::new(error_trace_id, "span-c-recovery", true, "")
  let span_c = azimuth::Span::new("service-c-operation", azimuth::Internal, span_c_ctx)
  azimuth::Span::set_status(span_c, azimuth::Ok)
  azimuth::Span::add_event(span_c, "Error recovered", Some([("recovery.action", azimuth::StringValue("fallback"))]))
  
  // 验证错误处理的一致性
  assert_eq(azimuth::SpanContext::trace_id(span_a_ctx), error_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(span_b_ctx), error_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(span_c_ctx), error_trace_id)
  
  // 验证Span状态
  assert_eq(azimuth::Span::status(span_a), azimuth::Error)
  assert_eq(azimuth::Span::status(span_b), azimuth::Error)
  assert_eq(azimuth::Span::status(span_c), azimuth::Ok)
  
  // 验证Span Kind
  assert_eq(azimuth::Span::kind(span_a), azimuth::Server)
  assert_eq(azimuth::Span::kind(span_b), azimuth::Client)
  assert_eq(azimuth::Span::kind(span_c), azimuth::Internal)
}