// 跨模块集成测试 - 测试不同API之间的协作
use azimuth.telemetry.api.common.{AttributeValue, Resource}
use azimuth.telemetry.api.context.{Context, create_key}
use azimuth.telemetry.api.logs.{LogRecord, SeverityNumber, NoopLogger}
use azimuth.telemetry.api.metrics.{NoopMeter, Measurement}
use azimuth.telemetry.api.trace.{Span, SpanKind, StatusCode}
use azimuth.telemetry.api.propagation.{MapCarrier, W3CTraceContextPropagator}

test "cross_module_integration_basic" {
  // 测试基本的跨模块集成
  
  // 1. 创建Resource和Context
  let resource = Resource::default("integration-test-service")
  let context = Context::empty()
  let key = create_key("test-key")
  let context_with_value = context.with_value(key, "test-value")
  
  // 2. 测试Context与LogRecord的集成
  let log_record = LogRecord::builder()
    .severity(SeverityNumber::Info)
    .body("Integration test log message")
    .with_attribute("service.name", AttributeValue::string(resource.service_name))
    .build()
  
  assert_eq(log_record.severity_number, SeverityNumber::Info)
  assert_eq(log_record.body.unwrap_or(""), "Integration test log message")
  assert_eq(log_record.attributes.length(), 1)
  
  // 3. 测试Logger与Context的集成
  let logger = NoopLogger::{}
  logger.info("Test log message", [])
  
  // 4. 测试Meter与Measurement的集成
  let meter = NoopMeter::{}
  let counter = meter.create_counter("test-counter", "count", "Test counter")
  counter.add(1L, [("test.attribute", AttributeValue::string("test-value"))])
  
  // 5. 测试Tracer与Context的集成
  let tracer = NoopTracer::{}
  let (span_context, span) = tracer.start_span(
    context_with_value,
    "integration-test-span",
    Some(SpanKind::Internal),
    [("integration.test", AttributeValue::bool(true))],
    Some(12345L)
  )
  
  assert_eq(span.name, "integration-test-span")
  assert_eq(span.kind, SpanKind::Internal)
  assert_eq(span.attributes.length(), 1)
  
  // 6. 测试Propagation与Context的集成
  let propagator = W3CTraceContextPropagator::{}
  let carrier = MapCarrier::new()
  propagator.inject(context_with_value, carrier)
  
  let extracted_context = propagator.extract(context, carrier)
  match extracted_context.get(key) {
    Some(_) => assert_eq(true, true)
    None => assert_eq(false, true)
  }
}

test "cross_module_complex_workflow" {
  // 测试复杂的跨模块工作流
  
  // 1. 设置基础组件
  let resource = Resource::default("complex-workflow-service")
  let context = Context::empty()
  
  // 2. 在Context中设置多个值
  let user_key = create_key("user.id")
  let request_key = create_key("request.id")
  let trace_key = create_key("trace.id")
  
  let context_with_data = context
    .with_value(user_key, "user-123")
    .with_value(request_key, "req-456")
    .with_value(trace_key, "trace-789")
  
  // 3. 创建带有属性的LogRecord
  let log_record = LogRecord::builder()
    .severity(SeverityNumber::Warn)
    .body("Complex workflow operation")
    .with_attribute("user.id", AttributeValue::string("user-123"))
    .with_attribute("request.id", AttributeValue::string("req-456"))
    .with_attribute("operation.type", AttributeValue::string("complex_workflow"))
    .with_attribute("processing.time", AttributeValue::float(150.5))
    .with_attribute("success", AttributeValue::bool(true))
    .build()
  
  assert_eq(log_record.attributes.length(), 5)
  
  // 4. 创建多个Span并测试关联
  let tracer = NoopTracer::{}
  let (ctx1, span1) = tracer.start_span(
    context_with_data,
    "operation-1",
    Some(SpanKind::Server),
    [("operation.step", AttributeValue::int(1L))]
  )
  
  let (ctx2, span2) = tracer.start_span(
    ctx1,
    "operation-2",
    Some(SpanKind::Client),
    [("operation.step", AttributeValue::int(2L))]
  )
  
  let (ctx3, span3) = tracer.start_span(
    ctx2,
    "operation-3",
    Some(SpanKind::Internal),
    [("operation.step", AttributeValue::int(3L))]
  )
  
  assert_eq(span1.name, "operation-1")
  assert_eq(span2.name, "operation-2")
  assert_eq(span3.name, "operation-3")
  assert_eq(span1.kind, SpanKind::Server)
  assert_eq(span2.kind, SpanKind::Client)
  assert_eq(span3.kind, SpanKind::Internal)
  
  // 5. 创建Metrics并记录
  let meter = NoopMeter::{}
  let counter = meter.create_counter("operations.total", "count", "Total operations")
  let histogram = meter.create_histogram("operation.duration", "ms", "Operation duration")
  let gauge = meter.create_gauge("system.memory", "MB", "System memory usage")
  
  counter.add(1L, [("operation.type", AttributeValue::string("server"))])
  histogram.record(120.5, [("operation.step", AttributeValue::int(1L))])
  gauge.record(512.0, [("system.component", AttributeValue::string("heap"))])
  
  // 6. 测试Propagation的复杂场景
  let propagator = W3CTraceContextPropagator::{}
  let carrier = MapCarrier::new()
  
  // 注入上下文
  propagator.inject(context_with_data, carrier)
  
  // 提取上下文
  let extracted_context = propagator.extract(Context::empty(), carrier)
  
  // 验证提取的上下文包含原始数据
  match extracted_context.get(user_key) {
    Some(value) => assert_eq(value, "user-123")
    None => assert_eq(false, true)
  }
  
  match extracted_context.get(request_key) {
    Some(value) => assert_eq(value, "req-456")
    None => assert_eq(false, true)
  }
  
  match extracted_context.get(trace_key) {
    Some(value) => assert_eq(value, "trace-789")
    None => assert_eq(false, true)
  }
}

test "cross_module_error_handling" {
  // 测试跨模块错误处理
  
  // 1. 创建带有错误信息的LogRecord
  let error_log = LogRecord::builder()
    .severity(SeverityNumber::Error)
    .body("Operation failed with error")
    .with_attribute("error.type", AttributeValue::string("ValidationError"))
    .with_attribute("error.message", AttributeValue::string("Invalid input parameter"))
    .with_attribute("error.code", AttributeValue::int(400L))
    .with_attribute("error.stack", AttributeValue::string("at function1 (line 23)"))
    .build()
  
  assert_eq(error_log.severity_number, SeverityNumber::Error)
  assert_eq(error_log.attributes.length(), 4)
  
  // 2. 创建带有错误状态的Span
  let tracer = NoopTracer::{}
  let (error_ctx, error_span) = tracer.start_span(
    Context::empty(),
    "error-operation",
    Some(SpanKind::Internal),
    [("error.expected", AttributeValue::bool(false))]
  )
  
  // 在实际实现中，这里应该设置Span的状态为Error
  // 由于是Noop实现，我们只能验证基本属性
  assert_eq(error_span.name, "error-operation")
  assert_eq(error_span.status, Unset)
  
  // 3. 测试错误上下文的传播
  let propagator = W3CTraceContextPropagator::{}
  let error_carrier = MapCarrier::new()
  propagator.inject(error_ctx, error_carrier)
  
  let recovered_ctx = propagator.extract(Context::empty(), error_carrier)
  assert_eq(recovered_ctx.values.length(), error_ctx.values.length())
}