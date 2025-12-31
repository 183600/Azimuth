// Azimuth Telemetry - Cross API Integration Test
// 跨API集成测试 (Trace + Metrics + Logs)

test "cross_api_basic_integration" {
  // 测试Trace、Metrics、Logs三个API的基本集成
  let trace_provider = trace::NoopTracerProvider::{}
  let metrics_provider = metrics::NoopMeterProvider::{}
  let logs_provider = logs::NoopLoggerProvider::{}
  
  let tracer = trace_provider.get_tracer("integrated-service")
  let meter = metrics_provider.get_meter("integrated-service")
  let logger = logs_provider.get_logger("integrated-service")
  
  let ctx = context::Context::empty()
  
  // 创建span
  let (span_ctx, span) = tracer.start_span(ctx, "user-operation", Some(trace::Server))
  
  // 记录指标
  let request_counter = meter.create_counter("requests-total")
  let response_time_histogram = meter.create_histogram("response-time-ms")
  
  request_counter.add(1L, Some([
    ("operation", common::AttributeValue::string("user-operation")),
    ("status", common::AttributeValue::string("success"))
  ]))
  
  response_time_histogram.record(150.5, Some([
    ("operation", common::AttributeValue::string("user-operation")),
    ("endpoint", common::AttributeValue::string("/api/users"))
  ]))
  
  // 记录日志
  logger.info("Operation completed successfully", Some([
    ("operation", common::AttributeValue::string("user-operation")),
    ("duration_ms", common::AttributeValue::float(150.5)),
    ("user_id", common::AttributeValue::int(12345L))
  ]))
  
  // 验证所有API都正常工作
  assert_eq(span.name, "user-operation")
  assert_eq(span.kind, trace::Server)
  assert_type(request_counter, metrics::NoopCounter)
  assert_type(response_time_histogram, metrics::NoopHistogram)
  assert_type(logger, logs::NoopLogger)
}

test "cross_api_context_propagation" {
  // 测试跨API的上下文传播
  let trace_provider = trace::NoopTracerProvider::{}
  let metrics_provider = metrics::NoopMeterProvider::{}
  let logs_provider = logs::NoopLoggerProvider::{}
  
  let tracer = trace_provider.get_tracer("context-test")
  let meter = metrics_provider.get_meter("context-test")
  let logger = logs_provider.get_logger("context-test")
  
  // 创建带有上下文信息的span
  let ctx = context::Context::empty()
  let user_key = context::create_key("user-id")
  let request_key = context::create_key("request-id")
  let enriched_ctx = ctx
    .with_value(user_key, "user-123")
    .with_value(request_key, "req-456")
  
  let (span_ctx, span) = tracer.start_span(enriched_ctx, "context-aware-operation")
  
  // 在指标中使用上下文信息
  let counter = meter.create_counter("context-aware-requests")
  counter.add(1L, Some([
    ("user_id", common::AttributeValue::string("user-123")),
    ("request_id", common::AttributeValue::string("req-456")),
    ("operation", common::AttributeValue::string("context-aware-operation"))
  ]))
  
  // 在日志中使用上下文信息
  logger.info("Context-aware operation", Some([
    ("user_id", common::AttributeValue::string("user-123")),
    ("request_id", common::AttributeValue::string("req-456")),
    ("span_name", common::AttributeValue::string("context-aware-operation")),
    ("trace_id", common::AttributeValue::array_string([
      "01", "02", "03", "04", "05", "06", "07", "08",
      "09", "0A", "0B", "0C", "0D", "0E", "0F", "10"
    ]))
  ]))
  
  // 验证上下文信息在span中保持
  let retrieved_user_id = span_ctx.get(user_key)
  let retrieved_request_id = span_ctx.get(request_key)
  
  match retrieved_user_id {
    Some(id) => assert_eq(id, "user-123")
    None => assert(false, "Expected user_id in context")
  }
  
  match retrieved_request_id {
    Some(id) => assert_eq(id, "req-456")
    None => assert(false, "Expected request_id in context")
  }
}

test "cross_api_complex_workflow" {
  // 测试复杂工作流程中的API集成
  let trace_provider = trace::NoopTracerProvider::{}
  let metrics_provider = metrics::NoopMeterProvider::{}
  let logs_provider = logs::NoopLoggerProvider::{}
  
  let tracer = trace_provider.get_tracer("workflow-service")
  let meter = metrics_provider.get_meter("workflow-service")
  let logger = logs_provider.get_logger("workflow-service")
  
  let ctx = context::Context::empty()
  
  // 工作流程开始
  logger.info("Workflow started", Some([
    ("workflow_id", common::AttributeValue::string("wf-123")),
    ("user_id", common::AttributeValue::int(45678))
  ]))
  
  // 创建主span
  let (main_ctx, main_span) = tracer.start_span(ctx, "main-workflow", Some(trace::Server))
  
  // 记录工作流程开始指标
  let workflow_counter = meter.create_counter("workflows-started")
  workflow_counter.add(1L, Some([
    ("workflow_type", common::AttributeValue::string("data-processing")),
    ("user_tier", common::AttributeValue::string("premium"))
  ]))
  
  // 步骤1：数据验证
  logger.debug("Starting data validation", Some([
    ("step", common::AttributeValue::int(1)),
    ("workflow_id", common::AttributeValue::string("wf-123"))
  ]))
  
  let (step1_ctx, step1_span) = tracer.start_span(main_ctx, "data-validation", Some(trace::Internal))
  let validation_counter = meter.create_counter("validations-attempted")
  validation_counter.add(1L, Some([
    ("validation_type", common::AttributeValue::string("schema")),
    ("step", common::AttributeValue::int(1))
  ]))
  
  logger.info("Data validation completed", Some([
    ("step", common::AttributeValue::int(1)),
    ("validation_result", common::AttributeValue::string("success")),
    ("records_validated", common::AttributeValue::int(1000))
  ]))
  
  // 步骤2：数据处理
  logger.debug("Starting data processing", Some([
    ("step", common::AttributeValue::int(2)),
    ("workflow_id", common::AttributeValue::string("wf-123"))
  ]))
  
  let (step2_ctx, step2_span) = tracer.start_span(main_ctx, "data-processing", Some(trace::Internal))
  let processing_histogram = meter.create_histogram("processing-time-ms")
  processing_histogram.record(2500.0, Some([
    ("step", common::AttributeValue::int(2)),
    ("algorithm", common::AttributeValue::string("ml-model-v2"))
  ]))
  
  logger.info("Data processing completed", Some([
    ("step", common::AttributeValue::int(2)),
    ("records_processed", common::AttributeValue::int(1000)),
    ("processing_time_ms", common::AttributeValue::float(2500.0))
  ]))
  
  // 步骤3：结果存储
  logger.debug("Starting result storage", Some([
    ("step", common::AttributeValue::int(3)),
    ("workflow_id", common::AttributeValue::string("wf-123"))
  ]))
  
  let (step3_ctx, step3_span) = tracer.start_span(main_ctx, "result-storage", Some(trace::Internal))
  let storage_counter = meter.create_counter("records-stored")
  storage_counter.add(1000L, Some([
    ("storage_type", common::AttributeValue::string("database")),
    ("table", common::AttributeValue::string("processed_results"))
  ]))
  
  logger.info("Results stored successfully", Some([
    ("step", common::AttributeValue::int(3)),
    ("records_stored", common::AttributeValue::int(1000)),
    ("storage_location", common::AttributeValue::string("postgresql://db/results"))
  ]))
  
  // 工作流程完成
  let completion_histogram = meter.create_histogram("workflow-duration-ms")
  completion_histogram.record(5000.0, Some([
    ("workflow_type", common::AttributeValue::string("data-processing")),
    ("status", common::AttributeValue::string("success"))
  ]))
  
  logger.info("Workflow completed successfully", Some([
    ("workflow_id", common::AttributeValue::string("wf-123")),
    ("total_duration_ms", common::AttributeValue::float(5000.0)),
    ("total_steps", common::AttributeValue::int(3)),
    ("final_status", common::AttributeValue::string("success"))
  ]))
  
  // 验证所有span都创建成功
  assert_eq(main_span.name, "main-workflow")
  assert_eq(step1_span.name, "data-validation")
  assert_eq(step2_span.name, "data-processing")
  assert_eq(step3_span.name, "result-storage")
}

test "cross_api_error_handling" {
  // 测试跨API的错误处理
  let trace_provider = trace::NoopTracerProvider::{}
  let metrics_provider = metrics::NoopMeterProvider::{}
  let logs_provider = logs::NoopLoggerProvider::{}
  
  let tracer = trace_provider.get_tracer("error-test")
  let meter = metrics_provider.get_meter("error-test")
  let logger = logs_provider.get_logger("error-test")
  
  let ctx = context::Context::empty()
  
  // 记录错误开始
  logger.warn("Error condition detected", Some([
    ("error_code", common::AttributeValue::string("E001")),
    ("component", common::AttributeValue::string("auth-service"))
  ]))
  
  // 创建错误span
  let (error_ctx, error_span) = tracer.start_span(ctx, "error-operation", Some(trace::Internal))
  
  // 记录错误指标
  let error_counter = meter.create_counter("errors-total")
  error_counter.add(1L, Some([
    ("error_code", common::AttributeValue::string("E001")),
    ("component", common::AttributeValue::string("auth-service")),
    ("severity", common::AttributeValue::string("high"))
  ]))
  
  // 记录详细错误日志
  logger.error("Authentication failed", Some([
    ("error_code", common::AttributeValue::string("E001")),
    ("error_message", common::AttributeValue::string("Invalid token provided")),
    ("user_id", common::AttributeValue::int(12345)),
    ("ip_address", common::AttributeValue::string("192.168.1.100")),
    ("user_agent", common::AttributeValue::string(" suspicious-agent/1.0")),
    ("failure_reason", common::AttributeValue::string("token_expired")),
    ("retry_count", common::AttributeValue::int(3))
  ]))
  
  // 记录恢复尝试
  let recovery_counter = meter.create_up_down_counter("recovery-attempts")
  recovery_counter.add(1L, Some([
    ("error_code", common::AttributeValue::string("E001")),
    ("recovery_strategy", common::AttributeValue::string("token_refresh"))
  ]))
  
  logger.info("Recovery attempt initiated", Some([
    ("error_code", common::AttributeValue::string("E001")),
    ("recovery_strategy", common::AttributeValue::string("token_refresh")),
    ("attempt_number", common::AttributeValue::int(1))
  ]))
  
  // 记录最终状态
  let final_status_gauge = meter.create_gauge("operation-status")
  final_status_gauge.record(0.0, Some([  // 0.0 表示失败
    ("operation", common::AttributeValue::string("authentication")),
    ("final_status", common::AttributeValue::string("failed"))
  ]))
  
  logger.error("Operation failed after recovery attempt", Some([
    ("error_code", common::AttributeValue::string("E001")),
    ("final_status", common::AttributeValue::string("failed")),
    ("total_attempts", common::AttributeValue::int(4)),
    ("total_duration_ms", common::AttributeValue::float(1500.0))
  ]))
  
  // 验证错误处理流程
  assert_eq(error_span.name, "error-operation")
  assert_eq(error_span.kind, trace::Internal)
}

test "cross_api_performance_monitoring" {
  // 测试跨API的性能监控
  let trace_provider = trace::NoopTracerProvider::{}
  let metrics_provider = metrics::NoopMeterProvider::{}
  let logs_provider = logs::NoopLoggerProvider::{}
  
  let tracer = trace_provider.get_tracer("performance-test")
  let meter = metrics_provider.get_meter("performance-test")
  let logger = logs_provider.get_logger("performance-test")
  
  let ctx = context::Context::empty()
  
  // 性能测试开始
  let start_time = 1640995200000000000L  // 模拟时间戳
  
  logger.debug("Performance test started", Some([
    ("test_type", common::AttributeValue::string("load_test")),
    ("start_time", common::AttributeValue::int(start_time))
  ]))
  
  // 创建性能监控span
  let (perf_ctx, perf_span) = tracer.start_span(ctx, "performance-test", Some(trace::Internal))
  
  // 模拟多个操作并记录性能指标
  let operation_histogram = meter.create_histogram("operation-duration-ms")
  let throughput_counter = meter.create_counter("operations-completed")
  
  // 模拟操作1
  operation_histogram.record(100.0, Some([
    ("operation_type", common::AttributeValue::string("database_query")),
    ("query_complexity", common::AttributeValue::string("simple"))
  ]))
  throughput_counter.add(1L, Some([
    ("operation_type", common::AttributeValue::string("database_query"))
  ]))
  
  // 模拟操作2
  operation_histogram.record(250.0, Some([
    ("operation_type", common::AttributeValue::string("database_query")),
    ("query_complexity", common::AttributeValue::string("complex"))
  ]))
  throughput_counter.add(1L, Some([
    ("operation_type", common::AttributeValue::string("database_query"))
  ]))
  
  // 模拟操作3
  operation_histogram.record(50.0, Some([
    ("operation_type", common::AttributeValue::string("cache_lookup")),
    ("cache_hit", common::AttributeValue::bool(true))
  ]))
  throughput_counter.add(1L, Some([
    ("operation_type", common::AttributeValue::string("cache_lookup"))
  ]))
  
  // 记录资源使用情况
  let memory_gauge = meter.create_gauge("memory-usage-mb")
  let cpu_gauge = meter.create_gauge("cpu-usage-percent")
  
  memory_gauge.record(512.0, Some([
    ("component", common::AttributeValue::string("application")),
    ("memory_type", common::AttributeValue::string("heap"))
  ]))
  
  cpu_gauge.record(75.5, Some([
    ("component", common::AttributeValue::string("application")),
    ("core_count", common::AttributeValue::int(4))
  ]))
  
  // 记录性能摘要日志
  logger.info("Performance test summary", Some([
    ("total_operations", common::AttributeValue::int(3)),
    ("avg_duration_ms", common::AttributeValue::float(133.33)),
    ("max_duration_ms", common::AttributeValue::float(250.0)),
    ("min_duration_ms", common::AttributeValue::float(50.0)),
    ("memory_usage_mb", common::AttributeValue::float(512.0)),
    ("cpu_usage_percent", common::AttributeValue::float(75.5)),
    ("cache_hit_rate", common::AttributeValue::float(100.0))
  ]))
  
  // 验证性能监控
  assert_eq(perf_span.name, "performance-test")
}

test "cross_api_resource_sharing" {
  // 测试跨API的资源共享
  let trace_provider = trace::NoopTracerProvider::{}
  let metrics_provider = metrics::NoopMeterProvider::{}
  let logs_provider = logs::NoopLoggerProvider::{}
  
  // 创建共享资源
  let resource = common::Resource::default("shared-resource-service")
  let instrumentation_scope = common::InstrumentationScope::{
    name: "shared-instrumentation",
    version: Some("1.0.0"),
    schema_url: Some("http://example.com/shared-schema")
  }
  
  let tracer = trace_provider.get_tracer("shared-service")
  let meter = metrics_provider.get_meter("shared-service")
  let logger = logs_provider.get_logger("shared-service")
  
  let ctx = context::Context::empty()
  
  // 在所有API中使用共享资源信息
  let (shared_ctx, shared_span) = tracer.start_span(ctx, "shared-resource-operation")
  
  // 在指标中使用共享资源信息
  let shared_counter = meter.create_counter("shared-operations")
  shared_counter.add(1L, Some([
    ("service_name", common::AttributeValue::string(resource.service_name)),
    ("sdk_name", common::AttributeValue::string(resource.telemetry_sdk_name)),
    ("sdk_version", common::AttributeValue::string(resource.telemetry_sdk_version)),
    ("instrumentation_name", common::AttributeValue::string(instrumentation_scope.name))
  ]))
  
  // 在日志中使用共享资源信息
  logger.info("Shared resource operation", Some([
    ("service_name", common::AttributeValue::string(resource.service_name)),
    ("sdk_name", common::AttributeValue::string(resource.telemetry_sdk_name)),
    ("sdk_version", common::AttributeValue::string(resource.telemetry_sdk_version)),
    ("instrumentation_name", common::AttributeValue::string(instrumentation_scope.name)),
    ("instrumentation_version", common::AttributeValue::string(instrumentation_scope.version.unwrap_or("unknown"))),
    ("schema_url", common::AttributeValue::string(instrumentation_scope.schema_url.unwrap_or("unknown")))
  ]))
  
  // 验证资源共享
  assert_eq(shared_span.name, "shared-resource-operation")
  assert_eq(resource.service_name, "shared-resource-service")
  assert_eq(instrumentation_scope.name, "shared-instrumentation")
}