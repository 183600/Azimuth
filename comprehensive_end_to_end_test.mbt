// 综合端到端测试用例
// 测试遥测系统的完整工作流程，包括指标、日志、链路追踪和上下文传播

test "end_to_end_telemetry_workflow" {
  // 1. 初始化遥测系统组件
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  let tracer_provider = trace::NoopTracerProvider::{}
  
  // 2. 创建遥测工具
  let meter = meter_provider.get_meter("test-meter", "1.0.0")
  let logger = logger_provider.get_logger("test-logger", "1.0.0")
  let tracer = tracer_provider.get_tracer("test-tracer", "1.0.0")
  
  // 3. 创建上下文并设置传播信息
  let ctx = context::Context::empty()
  let trace_key = context::create_key("trace-id")
  let ctx_with_trace = ctx.with_value(trace_key, "test-trace-123")
  
  // 4. 开始链路追踪
  let (span_ctx, span) = tracer.start_span(
    ctx_with_trace,
    "test-operation",
    trace::Internal,
    [("operation-type", common::AttributeValue::string("test"))],
    1609459200000000000L  // 2021-01-01 00:00:00 UTC
  )
  
  // 5. 在span中记录指标
  let counter = meter.create_counter("test-counter", "count", "Test counter metric")
  counter.add(1L, [("service", common::AttributeValue::string("test-service"))])
  
  let histogram = meter.create_histogram("test-histogram", "ms", "Test histogram metric")
  histogram.record(100.0, [("endpoint", common::AttributeValue::string("/test"))])
  
  // 6. 在span中记录日志
  let log_record = logs::LogRecord::builder()
    .timestamp(1609459200000000000L)
    .severity(logs::Info)
    .body("Test log message")
    .with_attribute("span-id", common::AttributeValue::string("test-span-456"))
    .with_attribute("user-id", common::AttributeValue::int(12345L))
    .build()
  
  logger.emit(log_record)
  
  // 7. 使用便捷方法记录不同级别的日志
  logger.debug("Debug message", [("component", common::AttributeValue::string("auth"))])
  logger.info("Info message", [("component", common::AttributeValue::string("api"))])
  logger.warn("Warning message", [("component", common::AttributeValue::string("cache"))])
  logger.error("Error message", [("component", common::AttributeValue::string("database"))])
  
  // 8. 验证上下文传播
  let retrieved_trace_id = span_ctx.get(trace_key)
  @assertion.assert_eq(retrieved_trace_id?, "test-trace-123")
  
  // 9. 验证span属性
  @assertion.assert_eq(span.name, "test-operation")
  @assertion.assert_eq(span.kind, trace::Internal)
  @assertion.assert_eq(span.status, trace::Unset)
  
  // 10. 验证资源配置
  let resource = common::Resource::default("test-service")
  @assertion.assert_eq(resource.service_name, "test-service")
  @assertion.assert_eq(resource.telemetry_sdk_name, "azimuth")
  @assertion.assert_eq(resource.telemetry_sdk_version, "0.1.0")
  
  // 11. 测试baggage传播
  let baggage = context::Baggage::empty()
  let baggage_with_data = baggage
    .with_entry("user-id", "12345")
    .with_entry("session-id", "abcdef")
  
  let user_id = baggage_with_data.get("user-id")
  let session_id = baggage_with_data.get("session-id")
  
  @assertion.assert_eq(user_id?, "12345")
  @assertion.assert_eq(session_id?, "abcdef")
  
  // 12. 测试属性值类型转换
  let string_attr = common::AttributeValue::string("test")
  let int_attr = common::AttributeValue::int(42L)
  let float_attr = common::AttributeValue::float(3.14)
  let bool_attr = common::AttributeValue::bool(true)
  let array_attr = common::AttributeValue::array_string(["a", "b", "c"])
  
  // 验证属性值创建
  match string_attr {
    common::StringValue(s) => @assertion.assert_eq(s, "test")
    _ => @test.expect_failure("Expected string attribute")
  }
  
  match int_attr {
    common::IntValue(i) => @assertion.assert_eq(i, 42L)
    _ => @test.expect_failure("Expected int attribute")
  }
  
  match float_attr {
    common::FloatValue(f) => @assertion.assert_eq(f, 3.14)
    _ => @test.expect_failure("Expected float attribute")
  }
  
  match bool_attr {
    common::BoolValue(b) => @assertion.assert_eq(b, true)
    _ => @test.expect_failure("Expected bool attribute")
  }
  
  match array_attr {
    common::ArrayStringValue(arr) => {
      @assertion.assert_eq(arr.length(), 3)
      @assertion.assert_eq(arr[0], "a")
      @assertion.assert_eq(arr[1], "b")
      @assertion.assert_eq(arr[2], "c")
    }
    _ => @test.expect_failure("Expected array string attribute")
  }
}

test "telemetry_component_integration" {
  // 测试不同遥测组件之间的集成
  
  // 1. 创建共享资源
  let resource = common::Resource::default("integration-test")
  let instrumentation_scope = common::InstrumentationScope::{
    name: "test-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  
  // 2. 测试跨组件数据共享
  let ctx = context::Context::empty()
  let correlation_key = context::create_key("correlation-id")
  let ctx_with_correlation = ctx.with_value(correlation_key, "corr-123456")
  
  // 3. 在多个span中传播上下文
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("integration-tracer")
  
  let (ctx1, span1) = tracer.start_span(
    ctx_with_correlation,
    "parent-operation",
    trace::Server,
    [("service", common::AttributeValue::string("api"))]
  )
  
  let (ctx2, span2) = tracer.start_span(
    ctx1,
    "child-operation",
    trace::Internal,
    [("component", common::AttributeValue::string("database"))]
  )
  
  // 4. 验证上下文在span链中传播
  let correlation_in_span2 = ctx2.get(correlation_key)
  @assertion.assert_eq(correlation_in_span2?, "corr-123456")
  
  // 5. 测试日志与追踪关联
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("integration-logger")
  
  let correlated_log = logs::LogRecord::builder()
    .timestamp(1609459200000000000L)
    .severity(logs::Info)
    .body("Operation completed successfully")
    .with_attribute("correlation-id", common::AttributeValue::string("corr-123456"))
    .with_attribute("span-name", common::AttributeValue::string("child-operation"))
    .build()
  
  logger.emit(correlated_log)
  
  // 6. 测试指标与追踪关联
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("integration-meter")
  
  let operation_counter = meter.create_counter(
    "operations_total",
    "count",
    "Total number of operations"
  )
  
  operation_counter.add(
    1L,
    [
      ("operation", common::AttributeValue::string("child-operation")),
      ("correlation-id", common::AttributeValue::string("corr-123456")),
      ("status", common::AttributeValue::string("success"))
    ]
  )
  
  // 7. 验证所有组件使用相同的上下文信息
  @assertion.assert_eq(span1.name, "parent-operation")
  @assertion.assert_eq(span2.name, "child-operation")
  @assertion.assert_eq(span1.kind, trace::Server)
  @assertion.assert_eq(span2.kind, trace::Internal)
}