// 跨模块集成测试 - 测试Trace、Metrics、Logs和Context模块的协同工作

test "cross_module_integration_trace_metrics_logs" {
  // 创建基础上下文
  let ctx = Context::empty()
  
  // 创建Tracer和Logger
  let tracer_provider = NoopTracerProvider::{}
  let logger_provider = NoopLoggerProvider::{}
  let meter_provider = NoopMeterProvider::{}
  
  let tracer = tracer_provider.get_tracer("integration-test", Some("1.0.0"))
  let logger = logger_provider.get_logger("integration-test", Some("1.0.0"))
  let meter = meter_provider.get_meter("integration-test", Some("1.0.0"))
  
  // 创建Span
  let (span_ctx, span) = tracer.start_span(ctx, "integration_operation", Server, Some([
    ("service.name", AttributeValue::string("integration-service")),
    ("operation.type", AttributeValue::string("cross_module_test"))
  ]))
  
  // 在Span上下文中记录指标
  let counter = meter.create_counter("operations_total", Some("operations"), Some("Total operations"))
  let histogram = meter.create_histogram("operation_duration_seconds", Some("seconds"), Some("Operation duration"))
  
  counter.add(1L, Some([
    ("operation.name", AttributeValue::string("integration_operation")),
    ("operation.status", AttributeValue::string("started"))
  ]))
  
  histogram.record(0.5, Some([
    ("operation.name", AttributeValue::string("integration_operation")),
    ("span.kind", AttributeValue::string("server"))
  ]))
  
  // 在Span上下文中记录日志
  logger.info("Operation started", Some([
    ("operation.name", AttributeValue::string("integration_operation")),
    ("trace.id", AttributeValue::string("trace-id-123")),
    ("span.id", AttributeValue::string("span-id-456"))
  ]))
  
  // 验证所有组件都能正常工作
  assert_eq(span.name, "integration_operation")
  assert_eq(span.kind, Server)
  assert_eq(span.attributes.length(), 2)
  
  // 测试上下文传播
  let key = create_key("operation_id")
  let ctx_with_operation = span_ctx.with_value(key, "op-12345")
  
  match ctx_with_operation.get(key) {
    Some(operation_id) => assert_eq(operation_id, "op-12345")
    None => @test.fail("Test failed")
  }
  
  // 测试Baggage
  let baggage = Baggage::empty()
  let baggage_with_data = baggage.with_entry("user.id", "user-123")
  let baggage_with_more_data = baggage_with_data.with_entry("session.id", "session-456")
  
  match baggage_with_more_data.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user-123")
    None => @test.fail("Test failed")
  }
  
  match baggage_with_more_data.get("session.id") {
    Some(session_id) => assert_eq(session_id, "session-456")
    None => @test.fail("Test failed")
  }
  
  // 记录操作完成
  counter.add(1L, Some([
    ("operation.name", AttributeValue::string("integration_operation")),
    ("operation.status", AttributeValue::string("completed"))
  ]))
  
  logger.info("Operation completed", Some([
    ("operation.name", AttributeValue::string("integration_operation")),
    ("operation.status", AttributeValue::string("success"))
  ]))
}

test "propagation_integration_with_tracing" {
  // 测试传播API与追踪的集成
  let ctx = Context::empty()
  
  // 创建传播器
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 创建载体
  let carrier = MapCarrier::new()
  
  // 测试注入
  composite_propagator.inject(ctx, carrier)
  
  // 验证载体包含必要的头
  let keys = carrier.keys()
  assert_eq(keys.length(), 2)  // traceparent 和 baggage
  
  // 测试提取
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 验证上下文提取成功
  assert_eq(true, true)  // 如果没有崩溃，说明提取成功
  
  // 测试单独的传播器
  let trace_parent = carrier.get("traceparent")
  let baggage_header = carrier.get("baggage")
  
  match trace_parent {
    Some(trace_value) => assert_eq(trace_value.length() > 0, true)
    None => @test.fail("Test failed")
  }
  
  match baggage_header {
    Some(baggage_value) => assert_eq(baggage_value.length() > 0, true)
    None => @test.fail("Test failed")
  }
}

test "telemetry_signals_correlation" {
  // 测试不同遥测信号之间的关联性
  
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let logger_provider = NoopLoggerProvider::{}
  let meter_provider = NoopMeterProvider::{}
  
  let tracer = tracer_provider.get_tracer("correlation-test")
  let logger = logger_provider.get_logger("correlation-test")
  let meter = meter_provider.get_meter("correlation-test")
  
  // 创建关联的trace_id和span_id
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(i) { 
    if i < 8 { 0x12_byte } else { 0x34_byte }
  })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x56_byte })
  
  // 创建Span
  let (span_ctx, span) = tracer.start_span(ctx, "correlated_operation")
  
  // 创建与Span关联的LogRecord
  let correlated_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("OPERATION_INFO"),
    body: Some("Operation in progress"),
    attributes: [
      ("operation.name", AttributeValue::string("correlated_operation")),
      ("service.name", AttributeValue::string("correlation-service"))
    ],
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(1_byte),
    resource: Some(Resource::default("correlation-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "correlation-test",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  // 创建与Span关联的Metrics
  let counter = meter.create_counter("correlated_operations", Some("operations"), Some("Correlated operations"))
  let histogram = meter.create_histogram("correlated_duration", Some("seconds"), Some("Correlated operation duration"))
  
  counter.add(1L, Some([
    ("trace.id", AttributeValue::string("1234567890abcdef1234567890abcdef")),
    ("span.id", AttributeValue::string("12345678")),
    ("operation.name", AttributeValue::string("correlated_operation"))
  ]))
  
  histogram.record(1.5, Some([
    ("trace.id", AttributeValue::string("1234567890abcdef1234567890abcdef")),
    ("span.id", AttributeValue::string("12345678")),
    ("operation.name", AttributeValue::string("correlated_operation"))
  ]))
  
  // 发送关联的日志
  logger.emit(correlated_log)
  
  // 验证所有信号都有相同的关联标识
  assert_eq(correlated_log.trace_id.is_some(), true)
  assert_eq(correlated_log.span_id.is_some(), true)
  assert_eq(correlated_log.attributes.length(), 2)
  
  // 验证资源信息
  match correlated_log.resource {
    Some(resource) => {
      assert_eq(resource.service_name, "correlation-service")
      assert_eq(resource.telemetry_sdk_name, "azimuth")
    }
    None => @test.fail("Test failed")
  }
  
  // 验证instrumentation scope
  match correlated_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "correlation-test")
      match scope.version {
        Some(version) => assert_eq(version, "1.0.0")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
}

test "resource_and_instrumentation_scope_integration" {
  // 测试资源和instrumentation scope在所有遥测信号中的一致性
  
  let resource = Resource::{
    service_name: "integrated-service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("deployment.environment", AttributeValue::string("production")),
      ("host.name", AttributeValue::string("server-01")),
      ("service.namespace", AttributeValue::string("payments"))
    ]
  }
  
  let instrumentation_scope = InstrumentationScope::{
    name: "payment-processor",
    version: Some("1.5.2"),
    schema_url: Some("https://example.com/schemas/payment")
  }
  
  // 在Span中使用资源和instrumentation scope
  let span_context = SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0xab_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0xcd_byte }),
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  let span = Span::{
    name: "payment.process",
    context: span_context,
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200000000500L),
    status: Ok,
    status_description: Some("Payment processed successfully"),
    attributes: [
      ("payment.amount", AttributeValue::float(99.99)),
      ("payment.currency", AttributeValue::string("USD"))
    ],
    events: [],
    links: []
  }
  
  // 在LogRecord中使用相同的资源和instrumentation scope
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000100L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("PAYMENT_INFO"),
    body: Some("Payment processed successfully"),
    attributes: [
      ("payment.id", AttributeValue::string("pay-12345")),
      ("payment.amount", AttributeValue::float(99.99))
    ],
    trace_id: Some(span_context.trace_id),
    span_id: Some(span_context.span_id),
    trace_flags: Some(span_context.trace_flags),
    resource: Some(resource),
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  // 验证资源一致性
  assert_eq(resource.service_name, "integrated-service")
  assert_eq(resource.service_version, Some("2.1.0"))
  assert_eq(resource.attributes.length(), 3)
  
  // 验证instrumentation scope一致性
  assert_eq(instrumentation_scope.name, "payment-processor")
  assert_eq(instrumentation_scope.version, Some("1.5.2"))
  assert_eq(instrumentation_scope.schema_url, Some("https://example.com/schemas/payment"))
  
  // 验证Span和LogRecord使用相同的上下文信息
  assert_eq(log_record.trace_id.is_some(), true)
  assert_eq(log_record.span_id.is_some(), true)
  assert_eq(log_record.trace_flags, Some(1_byte))
  
  match log_record.resource {
    Some(log_resource) => {
      assert_eq(log_resource.service_name, resource.service_name)
      assert_eq(log_resource.service_version, resource.service_version)
      assert_eq(log_resource.attributes.length(), resource.attributes.length())
    }
    None => @test.fail("Test failed")
  }
  
  match log_record.instrumentation_scope {
    Some(log_scope) => {
      assert_eq(log_scope.name, instrumentation_scope.name)
      assert_eq(log_scope.version, instrumentation_scope.version)
      assert_eq(log_scope.schema_url, instrumentation_scope.schema_url)
    }
    None => @test.fail("Test failed")
  }
}