// Azimuth 核心遥测功能测试用例
// 专注于遥测系统的核心功能测试

// 测试1: 基础遥测指标收集
test "基础遥测指标收集测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "core.telemetry")
  
  // 创建计数器指标
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  Counter::add(request_counter, 100.0)
  
  // 创建直方图指标
  let latency_histogram = Meter::create_histogram(meter, "http.request.duration")
  Histogram::record(latency_histogram, 0.125)
  Histogram::record(latency_histogram, 0.250)
  Histogram::record(latency_histogram, 0.075)
  
  // 创建仪表指标
  let active_connections = Meter::create_gauge(meter, "http.connections.active")
  Gauge::set(active_connections, 25.0)
  
  // 验证指标创建
  assert_eq(Counter::value(request_counter), 100.0)
  assert_eq(Gauge::value(active_connections), 25.0)
}

// 测试2: 分布式追踪上下文管理
test "分布式追踪上下文管理测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "distributed.tracer")
  
  // 创建根span
  let root_span = Tracer::start_span(tracer, "api.request")
  let root_context = Span::context(root_span)
  
  // 创建子span
  let db_span = Tracer::start_span_with_context(tracer, "database.query", root_context)
  Span::set_attribute(db_span, "db.statement", StringValue("SELECT * FROM users"))
  Span::set_attribute(db_span, "db.type", StringValue("postgresql"))
  
  // 添加事件
  Span::add_event(db_span, "query.executed", None)
  Span::set_status(db_span, Ok)
  Span::end(db_span)
  
  // 完成根span
  Span::set_status(root_span, Ok)
  Span::end(root_span)
  
  // 验证span状态
  assert_eq(Span::status(root_span), Ok)
  assert_eq(Span::status(db_span), Ok)
}

// 测试3: 结构化日志记录
test "结构化日志记录测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "structured.logger")
  
  // 创建日志记录器
  let log_record = Logger::create_log_record(logger)
  LogRecord::set_severity(log_record, Info)
  LogRecord::set_body(log_record, "User authentication successful")
  LogRecord::set_attribute(log_record, "user.id", StringValue("user-12345"))
  LogRecord::set_attribute(log_record, "auth.method", StringValue("oauth2"))
  LogRecord::set_attribute(log_record, "response_time_ms", IntValue(150))
  
  // 发送日志
  Logger::emit(log_record)
  
  // 验证日志属性
  assert_eq(LogRecord::severity(log_record), Info)
  assert_eq(LogRecord::body(log_record), Some("User authentication successful"))
}

// 测试4: 资源属性管理
test "资源属性管理测试" {
  // 创建基础资源
  let resource = Resource::new()
  
  // 添加服务属性
  let service_attrs = [
    ("service.name", StringValue("payment.service")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("instance-abc123"))
  ]
  let service_resource = Resource::with_attributes(resource, service_attrs)
  
  // 添加主机属性
  let host_attrs = [
    ("host.name", StringValue("prod-web-01")),
    ("host.arch", StringValue("amd64")),
    ("os.type", StringValue("linux"))
  ]
  let host_resource = Resource::with_attributes(resource, host_attrs)
  
  // 合并资源
  let merged_resource = Resource::merge(service_resource, host_resource)
  
  // 验证资源属性
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("payment.service")))
  assert_eq(Resource::get_attribute(merged_resource, "host.name"), Some(StringValue("prod-web-01")))
}

// 测试5: 上下文传播机制
test "上下文传播机制测试" {
  // 创建根上下文
  let root_ctx = Context::root()
  
  // 添加追踪上下文
  let trace_ctx = SpanContext::new("trace-789", "span-456", true, "key1=value1")
  let ctx_with_trace = Context::with_span_context(root_ctx, trace_ctx)
  
  // 添加行李项
  let baggage_key = ContextKey::new("user.id")
  let ctx_with_baggage = Context::with_value(ctx_with_trace, baggage_key, "user-123")
  
  // 创建传播器
  let propagator = W3CTraceContextPropagator::new()
  let carrier = TextMapCarrier::new()
  
  // 注入上下文
  CompositePropagator::inject(propagator, ctx_with_baggage, carrier)
  
  // 提取上下文
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  let extracted_user_id = Context::get(extracted_ctx, baggage_key)
  
  // 验证传播结果
  assert_eq(extracted_user_id, Some("user-123"))
}

// 测试6: 错误处理和恢复
test "错误处理和恢复测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error.handler")
  
  // 创建错误span
  let error_span = Tracer::start_span(tracer, "operation.with.error")
  
  // 记录错误
  Span::set_status(error_span, Error)
  Span::add_event(error_span, "exception.occurred", None)
  Span::set_attribute(error_span, "error.type", StringValue("DatabaseConnectionError"))
  Span::set_attribute(error_span, "error.message", StringValue("Connection timeout"))
  Span::set_attribute(error_span, "error.retry_count", IntValue(3))
  
  // 创建恢复span
  let recovery_span = Tracer::start_span(tracer, "recovery.operation")
  Span::set_attribute(recovery_span, "recovery.strategy", StringValue("circuit_breaker"))
  Span::set_status(recovery_span, Ok)
  Span::end(recovery_span)
  
  // 完成错误span
  Span::end(error_span)
  
  // 验证错误处理
  assert_eq(Span::status(error_span), Error)
  assert_eq(Span::status(recovery_span), Ok)
}

// 测试7: 性能监控和基准测试
test "性能监控和基准测试测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "performance.monitor")
  
  // 创建性能指标
  let cpu_usage = Meter::create_gauge(meter, "system.cpu.usage")
  let memory_usage = Meter::create_gauge(meter, "system.memory.usage")
  let throughput = Meter::create_counter(meter, "operations.throughput")
  let latency = Meter::create_histogram(meter, "operation.latency")
  
  // 记录性能数据
  Gauge::set(cpu_usage, 75.5)
  Gauge::set(memory_usage, 60.2)
  Counter::add(throughput, 1000.0)
  Histogram::record(latency, 0.025)
  Histogram::record(latency, 0.050)
  Histogram::record(latency, 0.015)
  
  // 创建性能属性
  let perf_attrs = Attributes::new()
  Attributes::set(perf_attrs, "performance.benchmark", StringValue("api.v2.endpoint"))
  Attributes::set(perf_attrs, "performance.target.p99", FloatValue(0.100))
  Attributes::set(perf_attrs, "performance.actual.p99", FloatValue(0.085))
  Attributes::set(perf_attrs, "performance.passed", BoolValue(true))
  
  // 验证性能指标
  assert_eq(Gauge::value(cpu_usage), 75.5)
  assert_eq(Gauge::value(memory_usage), 60.2)
  assert_eq(Counter::value(throughput), 1000.0)
}

// 测试8: 数据序列化和反序列化
test "数据序列化和反序列化测试" {
  // 创建测试数据
  let test_span = Span::new("test.span", Client, SpanContext::new("trace-123", "span-456", true, ""))
  Span::set_attribute(test_span, "http.method", StringValue("GET"))
  Span::set_attribute(test_span, "http.url", StringValue("https://api.example.com/users"))
  Span::set_attribute(test_span, "http.status_code", IntValue(200))
  
  // 序列化span数据
  let serialized_data = Span::serialize(test_span)
  
  // 反序列化span数据
  let deserialized_span = Span::deserialize(serialized_data)
  
  // 验证序列化结果
  assert_eq(Span::name(deserialized_span), Span::name(test_span))
  assert_eq(Span::kind(deserialized_span), Span::kind(test_span))
  
  // 验证属性
  let original_method = Span::get_attribute(test_span, "http.method")
  let deserialized_method = Span::get_attribute(deserialized_span, "http.method")
  assert_eq(original_method, deserialized_method)
}

// 测试9: 配置管理和动态更新
test "配置管理和动态更新测试" {
  // 创建配置管理器
  let config_manager = TelemetryConfig::new()
  
  // 设置初始配置
  TelemetryConfig::set_sampling_rate(config_manager, 0.1)
  TelemetryConfig::set_exporter_endpoint(config_manager, "https://otel-collector.example.com:4317")
  TelemetryConfig::set_batch_size(config_manager, 512)
  TelemetryConfig::set_export_timeout(config_manager, 30000)
  
  // 验证初始配置
  assert_eq(TelemetryConfig::get_sampling_rate(config_manager), 0.1)
  assert_eq(TelemetryConfig::get_exporter_endpoint(config_manager), "https://otel-collector.example.com:4317")
  assert_eq(TelemetryConfig::get_batch_size(config_manager), 512)
  assert_eq(TelemetryConfig::get_export_timeout(config_manager), 30000)
  
  // 动态更新配置
  TelemetryConfig::update_sampling_rate(config_manager, 0.5)
  TelemetryConfig::update_batch_size(config_manager, 1024)
  
  // 验证更新后的配置
  assert_eq(TelemetryConfig::get_sampling_rate(config_manager), 0.5)
  assert_eq(TelemetryConfig::get_batch_size(config_manager), 1024)
}

// 测试10: 多租户隔离和安全性
test "多租户隔离和安全性测试" {
  // 创建租户A的上下文
  let tenant_a_ctx = Context::root()
  let tenant_a_key = ContextKey::new("tenant.id")
  let tenant_a_ctx_with_id = Context::with_value(tenant_a_ctx, tenant_a_key, "tenant-a-123")
  
  // 创建租户B的上下文
  let tenant_b_ctx = Context::root()
  let tenant_b_key = ContextKey::new("tenant.id")
  let tenant_b_ctx_with_id = Context::with_value(tenant_b_ctx, tenant_b_key, "tenant-b-456")
  
  // 创建隔离的资源
  let tenant_a_resource = Resource::with_attributes(Resource::new(), [
    ("tenant.id", StringValue("tenant-a-123")),
    ("tenant.plan", StringValue("enterprise")),
    ("tenant.quota.metrics", IntValue(1000000))
  ])
  
  let tenant_b_resource = Resource::with_attributes(Resource::new(), [
    ("tenant.id", StringValue("tenant-b-456")),
    ("tenant.plan", StringValue("standard")),
    ("tenant.quota.metrics", IntValue(100000))
  ])
  
  // 验证租户隔离
  let tenant_a_id = Context::get(tenant_a_ctx_with_id, tenant_a_key)
  let tenant_b_id = Context::get(tenant_b_ctx_with_id, tenant_b_key)
  
  assert_eq(tenant_a_id, Some("tenant-a-123"))
  assert_eq(tenant_b_id, Some("tenant-b-456"))
  assert_ne(tenant_a_id, tenant_b_id)
  
  // 验证资源隔离
  let tenant_a_quota = Resource::get_attribute(tenant_a_resource, "tenant.quota.metrics")
  let tenant_b_quota = Resource::get_attribute(tenant_b_resource, "tenant.quota.metrics")
  
  assert_eq(tenant_a_quota, Some(IntValue(1000000)))
  assert_eq(tenant_b_quota, Some(IntValue(100000)))
}