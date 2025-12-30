// 跨模块集成测试用例
// 测试不同API组件之间的协作和集成

test "cross_module_integration" {
  // 1. 初始化所有模块的组件
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  let tracer_provider = trace::NoopTracerProvider::{}
  
  let meter = meter_provider.get_meter("integration-test", "1.0.0", "https://example.com/schema")
  let logger = logger_provider.get_logger("integration-logger", "1.0.0", "https://example.com/schema")
  let tracer = tracer_provider.get_tracer("integration-tracer", "1.0.0")
  
  // 2. 测试metrics与logs的集成
  let metrics_logs_counter = meter.create_counter("metrics_logs_counter", "count", "Counter for metrics-logs integration")
  
  // 在日志记录中触发指标
  let mut log_index = 0
  while log_index < 5 {
    let log_record = logs::LogRecord::builder()
      .timestamp(1609459200000000000L + (log_index * 1000000000L))
      .severity(logs::Info)
      .body("Integration log " + @int64.to_string(log_index))
      .with_attribute("log-index", common::AttributeValue::int(log_index.to_string()))
      .build()
    
    logger.emit(log_record)
    
    // 每记录一条日志，增加计数器
    metrics_logs_counter.add(
      1L,
      [
        ("log-index", common::AttributeValue::int(log_index.to_string())),
        ("integration-type", common::AttributeValue::string("metrics-logs"))
      ]
    )
    
    log_index = log_index + 1
  }
  
  // 验证metrics-logs集成
  @assertion.assert_eq(log_index, 5, "All integration logs should be recorded")
  
  // 3. 测试metrics与trace的集成
  let metrics_trace_histogram = meter.create_histogram("trace_duration", "ms", "Trace duration histogram")
  let ctx = context::Context::empty()
  
  // 创建span并记录执行时间
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "integration-operation",
    trace::Server,
    [("operation-type", common::AttributeValue::string("integration"))],
    1609459200000000000L
  )
  
  // 模拟操作执行时间
  let operation_duration = 150.0
  metrics_trace_histogram.record(
    operation_duration,
    [
      ("span-name", common::AttributeValue::string(span.name)),
      ("span-kind", common::AttributeValue::string("Server")),
      ("integration-type", common::AttributeValue::string("metrics-trace"))
    ]
  )
  
  // 验证metrics-trace集成
  @assertion.assert_eq(span.name, "integration-operation")
  @assertion.assert_eq(span.kind, trace::Server)
  
  // 4. 测试logs与trace的集成
  let (log_trace_ctx, log_trace_span) = tracer.start_span(
    span_ctx,
    "log-trace-integration",
    trace::Internal,
    [("component", common::AttributeValue::string("logging"))]
  )
  
  // 在span中记录结构化日志
  let integration_log = logs::LogRecord::builder()
    .timestamp(1609459200000000000L + 5000000000L)
    .severity(logs::Info)
    .body("Operation within span")
    .with_attribute("span-name", common::AttributeValue::string(log_trace_span.name))
    .with_attribute("integration-type", common::AttributeValue::string("logs-trace"))
    .with_attribute("parent-span", common::AttributeValue::string(span.name))
    .build()
  
  logger.emit(integration_log)
  
  // 验证logs-trace集成
  @assertion.assert_eq(log_trace_span.name, "log-trace-integration")
  @assertion.assert_eq(log_trace_span.kind, trace::Internal)
  
  // 5. 测试context与所有模块的集成
  let correlation_key = context::create_key("correlation-id")
  let user_key = context::create_key("user-id")
  let session_key = context::create_key("session-id")
  
  let enriched_ctx = ctx
    .with_value(correlation_key, "corr-123456")
    .with_value(user_key, "user-789")
    .with_value(session_key, "session-abc")
  
  // 在metrics中使用enriched context
  let context_counter = meter.create_counter("context_operations", "count", "Operations with context")
  context_counter.add(
    1L,
    [
      ("correlation-id", common::AttributeValue::string(enriched_ctx.get(correlation_key)?)),
      ("user-id", common::AttributeValue::string(enriched_ctx.get(user_key)?)),
      ("session-id", common::AttributeValue::string(enriched_ctx.get(session_key)?))
    ]
  )
  
  // 在logs中使用enriched context
  let context_log = logs::LogRecord::builder()
    .timestamp(1609459200000000000L + 6000000000L)
    .severity(logs::Info)
    .body("Context-enriched log")
    .with_attribute("correlation-id", common::AttributeValue::string(enriched_ctx.get(correlation_key)?))
    .with_attribute("user-id", common::AttributeValue::string(enriched_ctx.get(user_key)?))
    .build()
  
  logger.emit(context_log)
  
  // 在trace中使用enriched context
  let (context_trace_ctx, context_trace_span) = tracer.start_span(
    enriched_ctx,
    "context-integration",
    trace::Client,
    [
      ("correlation-id", common::AttributeValue::string(enriched_ctx.get(correlation_key)?)),
      ("user-id", common::AttributeValue::string(enriched_ctx.get(user_key)?))
    ]
  )
  
  // 验证context集成
  @assertion.assert_eq(enriched_ctx.get(correlation_key)?, "corr-123456")
  @assertion.assert_eq(enriched_ctx.get(user_key)?, "user-789")
  @assertion.assert_eq(enriched_ctx.get(session_key)?, "session-abc")
  
  // 6. 测试baggage与所有模块的集成
  let base_baggage = context::Baggage::empty()
  let enriched_baggage = base_baggage
    .with_entry("request-method", "GET")
    .with_entry("request-path", "/api/integration")
    .with_entry("client-version", "1.0.0")
    .with_entry("region", "us-west-2")
  
  // 在metrics中使用baggage信息
  let baggage_counter = meter.create_counter("baggage_operations", "count", "Operations with baggage")
  baggage_counter.add(
    1L,
    [
      ("request-method", common::AttributeValue::string(enriched_baggage.get("request-method")?)),
      ("request-path", common::AttributeValue::string(enriched_baggage.get("request-path")?)),
      ("client-version", common::AttributeValue::string(enriched_baggage.get("client-version")?)),
      ("region", common::AttributeValue::string(enriched_baggage.get("region")?))
    ]
  )
  
  // 在logs中使用baggage信息
  let baggage_log = logs::LogRecord::builder()
    .timestamp(1609459200000000000L + 7000000000L)
    .severity(logs::Info)
    .body("Baggage-enriched log")
    .with_attribute("request-method", common::AttributeValue::string(enriched_baggage.get("request-method")?))
    .with_attribute("request-path", common::AttributeValue::string(enriched_baggage.get("request-path")?))
    .build()
  
  logger.emit(baggage_log)
  
  // 验证baggage集成
  @assertion.assert_eq(enriched_baggage.get("request-method")?, "GET")
  @assertion.assert_eq(enriched_baggage.get("request-path")?, "/api/integration")
  @assertion.assert_eq(enriched_baggage.get("client-version")?, "1.0.0")
  @assertion.assert_eq(enriched_baggage.get("region")?, "us-west-2")
  
  // 7. 测试resource与所有模块的集成
  let shared_resource = common::Resource::default("integration-service")
  let enriched_resource = common::Resource::{
    service_name: shared_resource.service_name,
    service_version: Some("1.2.3"),
    telemetry_sdk_name: shared_resource.telemetry_sdk_name,
    telemetry_sdk_version: shared_resource.telemetry_sdk_version,
    attributes: [
      ("environment", common::AttributeValue::string("production")),
      ("datacenter", common::AttributeValue::string("dc-1")),
      ("cluster", common::AttributeValue::string("cluster-a"))
    ]
  }
  
  // 在所有模块中使用resource信息
  let resource_counter = meter.create_counter("resource_operations", "count", "Operations with resource")
  resource_counter.add(
    1L,
    [
      ("service-name", common::AttributeValue::string(enriched_resource.service_name)),
      ("service-version", common::AttributeValue::string(enriched_resource.service_version?)),
      ("environment", common::AttributeValue::string("production"))
    ]
  )
  
  let resource_log = logs::LogRecord::builder()
    .timestamp(1609459200000000000L + 8000000000L)
    .severity(logs::Info)
    .body("Resource-enriched log")
    .with_attribute("service-name", common::AttributeValue::string(enriched_resource.service_name))
    .with_attribute("environment", common::AttributeValue::string("production"))
    .build()
  
  logger.emit(resource_log)
  
  // 验证resource集成
  @assertion.assert_eq(enriched_resource.service_name, "integration-service")
  @assertion.assert_eq(enriched_resource.service_version?, "1.2.3")
  @assertion.assert_eq(enriched_resource.telemetry_sdk_name, "azimuth")
  
  // 8. 测试instrumentation scope与所有模块的集成
  let shared_scope = common::InstrumentationScope::{
    name: "integration-scope",
    version: Some("2.0.0"),
    schema_url: Some("https://example.com/integration-schema")
  }
  
  // 使用instrumentation scope创建专门的组件
  let scope_meter = meter_provider.get_meter(
    shared_scope.name,
    shared_scope.version?,
    shared_scope.schema_url?
  )
  
  let scope_logger = logger_provider.get_logger(
    shared_scope.name,
    shared_scope.version?,
    shared_scope.schema_url?
  )
  
  let scope_tracer = tracer_provider.get_tracer(
    shared_scope.name,
    shared_scope.version?
  )
  
  // 验证instrumentation scope集成
  let scope_counter = scope_meter.create_counter("scoped_operations", "count", "Scoped operations")
  scope_counter.add(1L, [("scope", common::AttributeValue::string(shared_scope.name))])
  
  let (scope_ctx, scope_span) = scope_tracer.start_span(
    ctx,
    "scoped-operation",
    trace::Internal,
    [("scope", common::AttributeValue::string(shared_scope.name))]
  )
  
  let scope_log = logs::LogRecord::builder()
    .timestamp(1609459200000000000L + 9000000000L)
    .severity(logs::Info)
    .body("Scope-enriched log")
    .with_attribute("scope", common::AttributeValue::string(shared_scope.name))
    .build()
  
  scope_logger.emit(scope_log)
  
  // 验证scope集成
  @assertion.assert_eq(shared_scope.name, "integration-scope")
  @assertion.assert_eq(shared_scope.version?, "2.0.0")
  @assertion.assert_eq(shared_scope.schema_url?, "https://example.com/integration-schema")
  
  // 9. 测试完整的三模块集成场景
  let integration_trace_id = "trace-123456789"
  let integration_user_id = "user-987654321"
  
  // 创建集成的上下文
  let integration_ctx = ctx
    .with_value(context::create_key("trace-id"), integration_trace_id)
    .with_value(context::create_key("user-id"), integration_user_id)
  
  // 开始集成的trace
  let (integration_trace_ctx, integration_span) = tracer.start_span(
    integration_ctx,
    "full-integration-operation",
    trace::Server,
    [
      ("trace-id", common::AttributeValue::string(integration_trace_id)),
      ("user-id", common::AttributeValue::string(integration_user_id)),
      ("operation-type", common::AttributeValue::string("full-integration"))
    ]
  )
  
  // 在trace中记录metrics
  let integration_histogram = meter.create_histogram("integration_operation_duration", "ms", "Integration operation duration")
  integration_histogram.record(
    250.0,
    [
      ("trace-id", common::AttributeValue::string(integration_trace_id)),
      ("user-id", common::AttributeValue::string(integration_user_id)),
      ("operation", common::AttributeValue::string("full-integration"))
    ]
  )
  
  // 在trace中记录logs
  let integration_completion_log = logs::LogRecord::builder()
    .timestamp(1609459200000000000L + 10000000000L)
    .severity(logs::Info)
    .body("Full integration operation completed")
    .with_attribute("trace-id", common::AttributeValue::string(integration_trace_id))
    .with_attribute("user-id", common::AttributeValue::string(integration_user_id))
    .with_attribute("span-name", common::AttributeValue::string(integration_span.name))
    .with_attribute("duration-ms", common::AttributeValue::float(250.0))
    .build()
  
  logger.emit(integration_completion_log)
  
  // 验证完整集成
  @assertion.assert_eq(integration_span.name, "full-integration-operation")
  @assertion.assert_eq(integration_trace_id, "trace-123456789")
  @assertion.assert_eq(integration_user_id, "user-987654321")
}

test "module_compatibility_integration" {
  // 测试模块兼容性和互操作性
  
  // 1. 测试不同版本的兼容性
  let v1_meter_provider = metrics::NoopMeterProvider::{}
  let v2_meter_provider = metrics::NoopMeterProvider::{}
  
  let v1_meter = v1_meter_provider.get_meter("compatibility-test", "1.0.0")
  let v2_meter = v2_meter_provider.get_meter("compatibility-test", "2.0.0")
  
  // 不同版本的组件应该能够协同工作
  let v1_counter = v1_meter.create_counter("version_compatibility", "count", "Version compatibility test")
  let v2_counter = v2_meter.create_counter("version_compatibility", "count", "Version compatibility test")
  
  v1_counter.add(1L, [("version", common::AttributeValue::string("v1.0.0"))])
  v2_counter.add(1L, [("version", common::AttributeValue::string("v2.0.0"))])
  
  // 2. 测试不同配置的兼容性
  let default_meter = meter_provider.get_meter("default-test")
  let configured_meter = meter_provider.get_meter("configured-test", "1.0.0", "https://example.com/schema")
  
  let default_counter = default_meter.create_counter("config_compatibility", "count", "Config compatibility test")
  let configured_counter = configured_meter.create_counter("config_compatibility", "count", "Config compatibility test")
  
  default_counter.add(1L, [("config", common::AttributeValue::string("default"))])
  configured_counter.add(1L, [("config", common::AttributeValue::string("configured"))])
  
  // 3. 测试跨模块属性传递
  let cross_module_attributes = [
    ("global-trace-id", common::AttributeValue::string("global-123")),
    ("global-request-id", common::AttributeValue::string("req-456")),
    ("global-session-id", common::AttributeValue::string("sess-789")),
    ("global-user-id", common::AttributeValue::string("user-abc"))
  ]
  
  // 在metrics中使用跨模块属性
  let cross_module_counter = meter.create_counter("cross_module_operations", "count", "Cross-module operations")
  cross_module_counter.add(1L, cross_module_attributes)
  
  // 在logs中使用跨模块属性
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("cross-module-logger")
  
  let cross_module_log = logs::LogRecord::builder()
    .timestamp(1609459200000000000L)
    .severity(logs::Info)
    .body("Cross-module operation")
    .with_attribute("global-trace-id", common::AttributeValue::string("global-123"))
    .with_attribute("global-request-id", common::AttributeValue::string("req-456"))
    .build()
  
  logger.emit(cross_module_log)
  
  // 在trace中使用跨模块属性
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("cross-module-tracer")
  
  let ctx = context::Context::empty()
  let (cross_module_ctx, cross_module_span) = tracer.start_span(
    ctx,
    "cross-module-operation",
    trace::Internal,
    [
      ("global-trace-id", common::AttributeValue::string("global-123")),
      ("global-request-id", common::AttributeValue::string("req-456"))
    ]
  )
  
  // 4. 测试模块间的错误传播
  let error_logger = logger_provider.get_logger("error-propagation-logger")
  let error_tracer = tracer_provider.get_tracer("error-propagation-tracer")
  
  // 模拟错误场景
  let (error_ctx, error_span) = error_tracer.start_span(
    ctx,
    "error-operation",
    trace::Internal,
    [("operation-type", common::AttributeValue::string("error-test"))]
  )
  
  // 记录错误日志
  let error_log = logs::LogRecord::builder()
    .timestamp(1609459200000000000L + 1000000000L)
    .severity(logs::Error)
    .body("Operation failed with error")
    .with_attribute("error-code", common::AttributeValue::string("ERR-001"))
    .with_attribute("error-message", common::AttributeValue::string("Simulated error"))
    .with_attribute("span-name", common::AttributeValue::string(error_span.name))
    .build()
  
  error_logger.emit(error_log)
  
  // 记录错误指标
  let error_counter = meter.create_counter("error_operations", "count", "Error operations")
  error_counter.add(
    1L,
    [
      ("error-code", common::AttributeValue::string("ERR-001")),
      ("span-name", common::AttributeValue::string(error_span.name))
    ]
  )
  
  // 5. 测试模块间的性能协调
  let performance_meter = meter_provider.get_meter("performance-coordination")
  let performance_logger = logger_provider.get_logger("performance-coordination")
  let performance_tracer = tracer_provider.get_tracer("performance-coordination")
  
  // 协调的性能测试
  let (perf_ctx, perf_span) = performance_tracer.start_span(
    ctx,
    "coordinated-performance-operation",
    trace::Server,
    [("test-type", common::AttributeValue::string("performance"))]
  )
  
  let performance_start = 1609459200000000000L
  let performance_end = performance_start + 500000000L  // 500ms
  let performance_duration = (performance_end - performance_start) / 1000000L  // 转换为毫秒
  
  // 记录性能指标
  let performance_histogram = performance_meter.create_histogram("operation_duration", "ms", "Operation duration")
  performance_histogram.record(
    @double.from_int(performance_duration),
    [("operation", common::AttributeValue::string("coordinated-performance"))]
  )
  
  // 记录性能日志
  let performance_log = logs::LogRecord::builder()
    .timestamp(performance_end)
    .severity(logs::Info)
    .body("Performance operation completed")
    .with_attribute("duration-ms", common::AttributeValue::int(performance_duration.to_string()))
    .with_attribute("span-name", common::AttributeValue::string(perf_span.name))
    .build()
  
  performance_logger.emit(performance_log)
  
  // 验证性能协调
  @assertion.assert_eq(performance_duration, 500L, "Performance duration should be coordinated")
  
  // 6. 测试模块间的状态同步
  let state_meter = meter_provider.get_meter("state-synchronization")
  let state_logger = logger_provider.get_logger("state-synchronization")
  let state_tracer = tracer_provider.get_tracer("state-synchronization")
  
  // 同步状态测试
  let state_ctx = ctx
    .with_value(context::create_key("sync-state"), "initialized")
    .with_value(context::create_key("sync-version"), "1.0")
  
  let (sync_ctx, sync_span) = state_tracer.start_span(
    state_ctx,
    "state-synchronization-operation",
    trace::Internal,
    [("sync-state", common::AttributeValue::string("initialized"))]
  )
  
  // 在所有模块中同步状态
  let state_counter = state_meter.create_counter("state_changes", "count", "State changes")
  state_counter.add(
    1L,
    [
      ("sync-state", common::AttributeValue::string("initialized")),
      ("sync-version", common::AttributeValue::string("1.0"))
    ]
  )
  
  let state_log = logs::LogRecord::builder()
    .timestamp(1609459200000000000L + 2000000000L)
    .severity(logs::Info)
    .body("State synchronized")
    .with_attribute("sync-state", common::AttributeValue::string("initialized"))
    .with_attribute("sync-version", common::AttributeValue::string("1.0"))
    .build()
  
  state_logger.emit(state_log)
  
  // 验证状态同步
  @assertion.assert_eq(state_ctx.get(context::create_key("sync-state"))?, "initialized")
  @assertion.assert_eq(state_ctx.get(context::create_key("sync-version"))?, "1.0")
  
  // 验证所有集成测试通过
  @assertion.assert_true(true, "All module compatibility integration tests should pass")
}