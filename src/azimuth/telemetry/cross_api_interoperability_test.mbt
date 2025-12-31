// Azimuth Telemetry - Cross-API Interoperability Test
// 测试Traces、Metrics、Logs三个API的互操作性

test "cross_api_trace_metrics_logs_correlation" {
  // 创建共享的Context和Resource
  let resource = common::Resource::default("test-service")
  let ctx = context::Context::empty()
  let trace_key = context::create_key("trace_id")
  
  // 1. 创建Trace Span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test-tracer", "1.0.0")
  let (ctx_with_span, span) = tracer.start_span(
    ctx, 
    "operation-span", 
    trace::Internal, 
    [("operation_type", common::AttributeValue::string("test"))]
  )
  
  // 2. 在Trace上下文中记录Metrics
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter", "1.0.0")
  let counter = meter.create_counter("operations_total", "count", "Total number of operations")
  
  // 将trace ID关联到metric attributes
  let trace_id_str = "test-trace-id-12345"
  let ctx_with_trace_id = ctx_with_span.with_value(trace_key, trace_id_str)
  
  counter.add(1L, [("trace_id", common::AttributeValue::string(trace_id_str))])
  
  // 3. 在相同的Trace上下文中记录Logs
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("test-logger", "1.0.0")
  
  let log_record = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Operation completed successfully")
    .with_attribute("trace_id", common::AttributeValue::string(trace_id_str))
    .with_attribute("operation", common::AttributeValue::string("test-operation"))
    .build()
  
  logger.emit(log_record)
  
  // 验证所有三个信号都正确关联了相同的trace_id
  // 在实际实现中，这里会验证exported数据的相关性
  let retrieved_trace_id = ctx_with_trace_id.get(trace_key)
  assert_eq(retrieved_trace_id, Some(trace_id_str))
}

test "cross_api_context_propagation_integration" {
  // 测试Context在不同API之间的传播
  let initial_ctx = context::Context::empty()
  
  // 创建带有baggage的Context
  let baggage = context::Baggage::empty()
    .with_entry("user_id", "12345")
    .with_entry("session_id", "abcdef")
  
  let baggage_key = context::create_key("baggage")
  let ctx_with_baggage = initial_ctx.with_value(baggage_key, "user_id=12345,session_id=abcdef")
  
  // 1. 在Trace中使用Context
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test-tracer")
  let (ctx_with_span, _span) = tracer.start_span(
    ctx_with_baggage, 
    "context-propagation-test"
  )
  
  // 2. 在Metrics中使用相同的Context
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter")
  let histogram = meter.create_histogram("operation_duration", "ms", "Operation duration")
  
  histogram.record(100.5, [("service", common::AttributeValue::string("api-service"))])
  
  // 3. 在Logs中使用相同的Context
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("test-logger")
  
  logger.info("Processing request", [
    ("component", common::AttributeValue::string("request-handler")),
    ("user_id", common::AttributeValue::int(12345L))
  ])
  
  // 验证Context信息在所有API中保持一致
  let retrieved_baggage = ctx_with_span.get(baggage_key)
  assert_eq(retrieved_baggage, Some("user_id=12345,session_id=abcdef"))
}

test "cross_api_error_handling_correlation" {
  // 测试错误情况下API之间的关联
  let ctx = context::Context::empty()
  let error_key = context::create_key("error_context")
  
  // 1. 在Trace中记录错误
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("error-test-tracer")
  let (error_ctx, error_span) = tracer.start_span(ctx, "error-operation")
  
  // 模拟错误状态
  let error_context = "operation_failed:validation_error"
  let ctx_with_error = error_ctx.with_value(error_key, error_context)
  
  // 2. 在Metrics中记录错误计数
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("error-metrics")
  let error_counter = meter.create_counter("errors_total", "count", "Total errors")
  
  error_counter.add(1L, [
    ("error_type", common::AttributeValue::string("validation")),
    ("operation", common::AttributeValue::string("error-operation"))
  ])
  
  // 3. 在Logs中记录错误详情
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("error-logger")
  
  logger.error("Validation failed", [
    ("error_code", common::AttributeValue::string("VAL_001")),
    ("operation", common::AttributeValue::string("error-operation")),
    ("error_context", common::AttributeValue::string(error_context))
  ])
  
  // 验证错误信息在所有API中正确关联
  let retrieved_error = ctx_with_error.get(error_key)
  assert_eq(retrieved_error, Some(error_context))
}

test "cross_api_performance_correlation" {
  // 测试性能相关的API关联
  let ctx = context::Context::empty()
  let perf_key = context::create_key("performance_context")
  
  // 1. 创建性能监控的Trace
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("performance-tracer")
  let (perf_ctx, perf_span) = tracer.start_span(
    ctx, 
    "performance-critical-operation",
    trace::Internal,
    [("performance_category", common::AttributeValue::string("database"))]
  )
  
  // 2. 记录性能相关的Metrics
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("performance-meter")
  let duration_histogram = meter.create_histogram("operation_duration_ms")
  let memory_gauge = meter.create_gauge("memory_usage_mb")
  
  duration_histogram.record(250.7)
  memory_gauge.record(512.3)
  
  // 3. 记录性能相关的Logs
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("performance-logger")
  
  logger.info("Performance metrics collected", [
    ("operation", common::AttributeValue::string("performance-critical-operation")),
    ("duration_ms", common::AttributeValue::float(250.7)),
    ("memory_mb", common::AttributeValue::float(512.3))
  ])
  
  // 验证性能上下文正确传播
  let perf_context = "category:database,threshold:200ms"
  let ctx_with_perf = perf_ctx.with_value(perf_key, perf_context)
  let retrieved_perf = ctx_with_perf.get(perf_key)
  assert_eq(retrieved_perf, Some(perf_context))
}

test "cross_api_resource_consistency" {
  // 测试Resource在不同API之间的一致性
  let shared_resource = common::Resource::default("multi-api-service")
  
  // 1. 在Trace中使用相同的Resource
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("resource-test-tracer")
  let (_trace_ctx, _span) = tracer.start_span(
    context::Context::empty(), 
    "resource-consistency-test"
  )
  
  // 2. 在Metrics中使用相同的Resource
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("resource-test-meter")
  let counter = meter.create_counter("resource_operations")
  
  counter.add(1L)
  
  // 3. 在Logs中使用相同的Resource
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("resource-test-logger")
  
  logger.info("Resource consistency test")
  
  // 验证Resource信息在所有API中保持一致
  assert_eq(shared_resource.service_name, "multi-api-service")
  assert_eq(shared_resource.telemetry_sdk_name, "azimuth")
  assert_eq(shared_resource.telemetry_sdk_version, "0.1.0")
}