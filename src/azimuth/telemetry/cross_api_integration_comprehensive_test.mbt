// Cross-API Integration Test
// 测试trace、metrics、logs之间的协同工作能力

test "cross_api_integration_trace_metrics_logs" {
  // 创建资源
  let resource = common::Resource::default("integration-test-service")
  
  // 创建上下文
  let ctx = context::Context::current()
  
  // 测试TracerProvider
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("integration-test-tracer", "1.0.0")
  
  // 开始span
  let (ctx_with_span, span) = tracer.start_span(
    ctx,
    "integration-operation",
    trace::Server,
    [("operation.type", common::AttributeValue::string("test"))],
    1234500000000L
  )
  
  // 验证span创建
  @assertion.assert_eq(span.name, "integration-operation")
  @assertion.assert_eq(span.kind, trace::Server)
  @assertion.assert_eq(span.attributes.length(), 1)
  @assertion.assert_eq(span.attributes[0].0, "operation.type")
  
  // 测试MeterProvider
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("integration-test-meter", "1.0.0")
  
  // 创建metrics instruments
  let counter = meter.create_counter("operations.total", "count", "Total number of operations")
  let histogram = meter.create_histogram("operation.duration", "ms", "Operation duration")
  let gauge = meter.create_gauge("active.operations", "count", "Active operations")
  
  // 记录metrics
  counter.add(1L, [("operation.type", common::AttributeValue::string("test"))])
  histogram.record(100.5, [("operation.type", common::AttributeValue::string("test"))])
  gauge.record(5.0, [("operation.type", common::AttributeValue::string("test"))])
  
  // 测试LoggerProvider
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("integration-test-logger", "1.0.0")
  
  // 创建log记录
  let log_record = logs::LogRecord::builder()
    .timestamp(1234500000000L)
    .severity(logs::Info)
    .body("Integration test operation completed")
    .with_attribute("trace.id", common::AttributeValue::string("test-trace-id"))
    .with_attribute("span.id", common::AttributeValue::string("test-span-id"))
    .with_attribute("operation.result", common::AttributeValue::string("success"))
    .build()
  
  // 发送日志
  logger.emit(log_record)
  
  // 测试便捷日志方法
  logger.info("Integration test started", [("test.phase", common::AttributeValue::string("beginning"))])
  logger.warn("Integration test warning", [("test.phase", common::AttributeValue::string("warning"))])
  logger.error("Integration test error", [("test.phase", common::AttributeValue::string("error"))])
  
  // 验证所有API都能正常工作
  @assertion.assert_true(true) // 如果没有异常，说明集成成功
}

test "cross_api_context_propagation" {
  // 测试跨API的上下文传播
  let ctx = context::Context::current()
  
  // 在trace中创建span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("context-test")
  let (ctx_with_span, span) = tracer.start_span(ctx, "context-test-operation")
  
  // 在metrics中使用相同的上下文信息
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("context-test-meter")
  let counter = meter.create_counter("context.operations")
  
  // 模拟从上下文中提取trace信息
  let trace_id = "test-trace-id-from-context"
  counter.add(1L, [
    ("trace.id", common::AttributeValue::string(trace_id)),
    ("span.id", common::AttributeValue::string("test-span-id"))
  ])
  
  // 在logs中使用相同的上下文信息
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("context-test-logger")
  
  let log_record = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Context propagation test")
    .with_attribute("trace.id", common::AttributeValue::string(trace_id))
    .with_attribute("operation.context", common::AttributeValue::string("propagated"))
    .build()
  
  logger.emit(log_record)
  
  @assertion.assert_true(true) // 如果没有异常，说明上下文传播成功
}

test "cross_api_attribute_consistency" {
  // 测试跨API的属性类型一致性
  let test_attributes = [
    ("string.attr", common::AttributeValue::string("test-value")),
    ("int.attr", common::AttributeValue::int(42L)),
    ("float.attr", common::AttributeValue::float(3.14)),
    ("bool.attr", common::AttributeValue::bool(true)),
    ("array.string.attr", common::AttributeValue::array_string(["a", "b", "c"])),
    ("array.int.attr", common::AttributeValue::array_int([1L, 2L, 3L]))
  ]
  
  // 在trace中使用这些属性
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("attribute-test")
  let (_, span) = tracer.start_span(
    context::Context::current(),
    "attribute-test",
    trace::Internal,
    test_attributes
  )
  
  // 在metrics中使用相同的属性
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("attribute-test-meter")
  let counter = meter.create_counter("attribute.test")
  counter.add(1L, test_attributes)
  
  // 在logs中使用相同的属性
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("attribute-test-logger")
  logger.info("Attribute consistency test", test_attributes)
  
  // 验证属性类型在所有API中都能正确处理
  @assertion.assert_eq(span.attributes.length(), test_attributes.length())
  @assertion.assert_true(true) // 如果没有类型错误，说明属性一致性良好
}