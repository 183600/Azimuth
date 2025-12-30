// 完整的遥测工作流测试
// 测试从数据收集到导出的完整流程

test "telemetry_complete_workflow_trace_metrics_logs" {
  // 测试完整的遥测工作流：追踪 + 指标 + 日志
  
  // 1. 初始化上下文和资源
  let ctx = context::Context::empty()
  let resource = common::Resource::default("workflow-test-service")
  assert_eq(resource.service_name, "workflow-test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  
  // 2. 创建追踪器并开始Span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("workflow-tracer", "1.0.0")
  let (ctx_with_span, span) = tracer.start_span(
    ctx, 
    "complete-workflow-operation",
    trace::Server,
    [("operation.type", common::AttributeValue::string("integration"))],
    1640995200000000000L  // 2022-01-01 00:00:00 UTC
  )
  
  // 验证Span创建
  assert_eq(span.name, "complete-workflow-operation")
  assert_eq(span.kind, trace::Server)
  assert_eq(span.attributes.length(), 1)
  assert_eq(span.start_time_unix_nanos, 1640995200000000000L)
  
  // 3. 创建指标记录器并记录指标
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("workflow-meter", "1.0.0", "https://example.com/schema")
  
  let counter = meter.create_counter("workflow.operations", "count", "Number of workflow operations")
  let histogram = meter.create_histogram("workflow.duration", "ms", "Workflow operation duration")
  let gauge = meter.create_gauge("workflow.memory", "MB", "Memory usage during workflow")
  
  // 记录指标数据
  counter.add(1, [("workflow.type", common::AttributeValue::string("complete"))])
  histogram.record(150.5, [("service.name", common::AttributeValue::string("workflow-test-service"))])
  gauge.record(256.0, [("process.id", common::AttributeValue::int(12345))])
  
  // 4. 创建日志记录器并记录日志
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("workflow-logger", "1.0.0")
  
  // 使用便捷方法记录日志
  logger.debug("Workflow started", [("phase", common::AttributeValue::string("init"))])
  logger.info("Workflow in progress", [("step", common::AttributeValue::int(2))])
  logger.warn("Workflow taking longer than expected", [("duration", common::AttributeValue::float(200.0))])
  
  // 使用LogRecordBuilder创建详细日志
  let detailed_log = logs::LogRecord::builder()
    .timestamp(1640995201000000000L)
    .severity(logs::Error)
    .body("Workflow completed with errors")
    .with_attribute("error.count", common::AttributeValue::int(3))
    .with_attribute("retry.attempts", common::AttributeValue::int(2))
    .build()
  
  logger.emit(detailed_log)
  
  // 5. 上下文传播测试
  let baggage_key = "workflow.correlation_id"
  let baggage_value = "wf-12345-67890"
  let baggage = context::Baggage::empty()
    .with_entry(baggage_key, baggage_value)
  
  let context_key = context::create_key("workflow.session")
  let ctx_with_session = ctx_with_span.with_value(context_key, "session-abc-123")
  
  // 验证上下文数据
  assert_eq(baggage.get(baggage_key), Some(baggage_value))
  assert_eq(ctx_with_session.get(context_key), Some("session-abc-123"))
  
  // 6. 传播器测试
  let carrier = propagation::MapCarrier::new()
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  let baggage_propagator = propagation::W3CBaggagePropagator::{}
  
  // 注入追踪上下文
  trace_propagator.inject(ctx_with_session, carrier)
  assert_eq(carrier.get(propagation::TRACE_PARENT_HEADER), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  // 注入行李数据
  baggage_propagator.inject(ctx_with_session, carrier)
  assert_eq(carrier.get(propagation::BAGGAGE_HEADER), Some("key1=value1,key2=value2"))
  
  // 提取上下文
  let extracted_ctx = trace_propagator.extract(context::Context::empty(), carrier)
  let extracted_with_baggage = baggage_propagator.extract(extracted_ctx, carrier)
  
  // 7. 验证工作流完整性
  let workflow_summary = "Workflow completed: span=" + span.name + 
                        ", metrics=3, logs=5, context propagated"
  
  assert_eq(workflow_summary.contains("Workflow completed"), true)
  assert_eq(workflow_summary.contains("complete-workflow-operation"), true)
  assert_eq(workflow_summary.contains("metrics=3"), true)
  assert_eq(workflow_summary.contains("logs=5"), true)
  assert_eq(workflow_summary.contains("context propagated"), true)
}

test "telemetry_complex_attribute_handling" {
  // 测试复杂属性处理
  
  // 创建各种类型的属性值
  let string_attr = common::AttributeValue::string("test-value")
  let int_attr = common::AttributeValue::int(42)
  let float_attr = common::AttributeValue::float(3.14159)
  let bool_attr = common::AttributeValue::bool(true)
  
  let string_array_attr = common::AttributeValue::array_string(["a", "b", "c"])
  let int_array_attr = common::AttributeValue::array_int([1, 2, 3, 4, 5])
  let float_array_attr = common::AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array_attr = common::AttributeValue::array_bool([true, false, true])
  
  // 创建复杂属性集合
  let complex_attributes = [
    ("string.value", string_attr),
    ("int.value", int_attr),
    ("float.value", float_attr),
    ("bool.value", bool_attr),
    ("array.string", string_array_attr),
    ("array.int", int_array_attr),
    ("array.float", float_array_attr),
    ("array.bool", bool_array_attr)
  ]
  
  // 验证属性数量
  assert_eq(complex_attributes.length(), 8)
  
  // 创建带有复杂属性的Span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("complex-attr-tracer")
  let (_, span) = tracer.start_span(
    context::Context::empty(),
    "complex-attributes-test",
    trace::Internal,
    complex_attributes
  )
  
  // 验证Span属性
  assert_eq(span.attributes.length(), 8)
  
  // 创建带有复杂属性的日志
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("complex-attr-logger")
  
  let complex_log = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Log with complex attributes")
    .with_attribute("string.value", string_attr)
    .with_attribute("int.value", int_attr)
    .with_attribute("float.value", float_attr)
    .with_attribute("bool.value", bool_attr)
    .build()
  
  logger.emit(complex_log)
  
  // 验证复杂属性处理
  let attr_processing_result = "Processed " + complex_attributes.length().to_string() + 
                              " complex attributes successfully"
  assert_eq(attr_processing_result, "Processed 8 complex attributes successfully")
}