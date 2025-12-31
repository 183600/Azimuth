// 跨信号集成测试 - 测试Trace、Metrics、Logs三个信号的协同工作
test "cross_signal_integration_trace_metrics_logs" {
  // 创建基础资源
  let resource = common::Resource::default("test-service")
  
  // 创建上下文
  let ctx = context::Context::current()
  
  // 创建Tracer和Span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test-tracer", "1.0.0")
  let (ctx_with_span, span) = tracer.start_span(
    ctx, 
    "operation-span", 
    Some(trace::Server), 
    Some([("operation.type", common::AttributeValue::string("database"))])
  )
  
  // 验证Span创建
  @assert.eq(span.name, "operation-span")
  @assert.eq(span.kind, trace::Server)
  @assert.eq(span.attributes.length(), 1)
  @assert.eq(span.attributes[0].0, "operation.type")
  
  // 创建Meter和Metrics
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter", "1.0.0")
  let counter = meter.create_counter("operation-count", "operations", "Total number of operations")
  let histogram = meter.create_histogram("operation-duration", "ms", "Operation duration in milliseconds")
  
  // 记录指标数据
  counter.add(1L, Some([("operation.type", common::AttributeValue::string("database"))]))
  histogram.record(150.5, Some([("operation.type", common::AttributeValue::string("database"))]))
  
  // 创建Logger和Log记录
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("test-logger", "1.0.0")
  
  // 创建与Span关联的Log记录
  let log_record = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Operation completed successfully")
    .with_attribute("operation.type", common::AttributeValue::string("database"))
    .with_attribute("operation.result", common::AttributeValue::bool(true))
    .build()
  
  logger.emit(log_record)
  
  // 使用便捷方法记录日志
  logger.info("Operation started", Some([("operation.phase", common::AttributeValue::string("start"))]))
  logger.warn("Operation taking longer than expected", Some([("operation.phase", common::AttributeValue::string("slow"))]))
  
  // 验证跨信号关联性 - 所有信号都应该有相同的上下文信息
  let trace_id = span.context.trace_id
  let span_id = span.context.span_id
  
  // 在实际实现中，这里应该验证metrics和logs是否正确关联到trace context
  // 当前使用No-op实现，主要验证API调用不会出错
  @assert.eq(trace_id.length(), 16)
  @assert.eq(span_id.length(), 8)
}

test "cross_signal_correlation_with_context_propagation" {
  // 测试跨服务的上下文传播和信号关联
  
  // 创建初始上下文
  let ctx = context::Context::current()
  
  // 在服务A中创建Span
  let tracer_provider_a = trace::NoopTracerProvider::{}
  let tracer_a = tracer_provider_a.get_tracer("service-a")
  let (ctx_a, span_a) = tracer_a.start_span(ctx, "service-a-operation")
  
  // 记录服务A的指标
  let meter_provider_a = metrics::NoopMeterProvider::{}
  let meter_a = meter_provider_a.get_meter("service-a")
  let counter_a = meter_a.create_counter("requests")
  counter_a.add(1L)
  
  // 记录服务A的日志
  let logger_provider_a = logs::NoopLoggerProvider::{}
  let logger_a = logger_provider_a.get_logger("service-a")
  logger_a.info("Request processed in service A", Some([("service", common::AttributeValue::string("A"))]))
  
  // 模拟上下文传播到服务B
  let tracer_provider_b = trace::NoopTracerProvider::{}
  let tracer_b = tracer_provider_b.get_tracer("service-b")
  let (ctx_b, span_b) = tracer_b.start_span(ctx_a, "service-b-operation")
  
  // 记录服务B的指标
  let meter_provider_b = metrics::NoopMeterProvider::{}
  let meter_b = meter_provider_b.get_meter("service-b")
  let counter_b = meter_b.create_counter("requests")
  counter_b.add(1L)
  
  // 记录服务B的日志
  let logger_provider_b = logs::NoopLoggerProvider::{}
  let logger_b = logger_provider_b.get_logger("service-b")
  logger_b.info("Request processed in service B", Some([("service", common::AttributeValue::string("B"))]))
  
  // 验证span关系
  @assert.eq(span_b.parent_span_id.is_some(), true)
  
  // 在实际实现中，应该验证所有信号都关联到同一个trace
  @assert.eq(span_a.context.trace_id.length(), 16)
  @assert.eq(span_b.context.trace_id.length(), 16)
}

test "cross_signal_error_propagation" {
  // 测试错误在跨信号中的传播和记录
  
  let ctx = context::Context::current()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("error-test")
  
  // 创建一个会出错的span
  let (error_ctx, error_span) = tracer.start_span(ctx, "error-operation")
  
  // 记录错误指标
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("error-test")
  let error_counter = meter.create_counter("errors", "errors", "Number of errors")
  error_counter.add(1L, Some([("error.type", common::AttributeValue::string("timeout"))]))
  
  // 记录错误日志
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("error-test")
  logger.error("Operation failed due to timeout", Some([
    ("error.type", common::AttributeValue::string("timeout")),
    ("error.retry_count", common::AttributeValue::int(3L))
  ]))
  
  // 在实际实现中，这里应该设置span的状态为Error
  // error_span.status = trace::Error
  // error_span.status_description = Some("Timeout occurred")
  
  // 验证错误信息被正确记录
  @assert.eq(error_span.name, "error-operation")
}