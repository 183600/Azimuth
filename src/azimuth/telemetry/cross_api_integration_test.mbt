// 跨API集成测试 - 验证trace、metrics、logs和context的协同工作

test "cross_api_integration_trace_metrics_logs" {
  // 创建基础资源
  let resource = common::Resource::default("test-service")
  
  // 创建Context
  let ctx = context::Context::current()
  
  // 创建Tracer和相关Span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test-tracer", "1.0.0")
  let (ctx_with_span, span) = tracer.start_span(
    ctx, 
    "operation-span", 
    trace::Internal, 
    [("operation.type", common::AttributeValue::string("test"))],
    1000000L
  )
  
  // 验证Span创建
  @assert.eq(span.name, "operation-span")
  @assert.eq(span.kind, trace::Internal)
  @assert.eq(span.attributes.length(), 1)
  @assert.eq(span.attributes[0].0, "operation.type")
  
  // 创建Meter和指标
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter", "1.0.0", "http://example.com/schema")
  let counter = meter.create_counter("test-counter", "count", "Test counter for integration")
  let histogram = meter.create_histogram("test-histogram", "ms", "Test histogram for integration")
  
  // 记录指标
  counter.add(42L, [("span.id", common::AttributeValue::string("test-span"))])
  histogram.record(123.45, [("operation.type", common::AttributeValue::string("test"))])
  
  // 创建Logger和日志
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("test-logger", "1.0.0", "http://example.com/schema")
  
  // 创建包含trace和span上下文的日志记录
  let log_record = logs::LogRecord::builder()
    .timestamp(2000000L)
    .severity(logs::Info)
    .body("Integration test log message")
    .with_attribute("service.name", common::AttributeValue::string("test-service"))
    .with_attribute("trace.id", common::AttributeValue::array_string(span.context.trace_id.map(fn(b) { b.to_string() })))
    .with_attribute("span.id", common::AttributeValue::array_string(span.context.span_id.map(fn(b) { b.to_string() })))
    .build()
  
  logger.emit(log_record)
  
  // 使用便捷日志方法
  logger.info("Info message with context", [("test.key", common::AttributeValue::int(100))])
  logger.warn("Warning message", [("severity.level", common::AttributeValue::int(2))])
  
  // 验证日志记录结构
  @assert.eq(log_record.severity_number, logs::Info)
  @assert.eq(log_record.body.unwrap(), "Integration test log message")
  @assert.eq(log_record.attributes.length(), 3)
  
  // 测试Context传播
  let propagated_ctx = ctx_with_span
  let (final_ctx, child_span) = tracer.start_span(
    propagated_ctx,
    "child-operation",
    trace::Client,
    [("parent.operation", common::AttributeValue::string("operation-span"))]
  )
  
  // 验证子Span
  @assert.eq(child_span.name, "child-operation")
  @assert.eq(child_span.kind, trace::Client)
  @assert.eq(child_span.parent_span_id.unwrap(), span.context.span_id)
  
  // 测试资源关联
  let resource_with_attributes = common::Resource::{
    service_name: "enhanced-service",
    service_version: Some("2.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", common::AttributeValue::string("test")),
      ("region", common::AttributeValue::string("us-west-2"))
    ]
  }
  
  @assert.eq(resource_with_attributes.service_name, "enhanced-service")
  @assert.eq(resource_with_attributes.service_version.unwrap(), "2.0.0")
  @assert.eq(resource_with_attributes.attributes.length(), 2)
}

test "cross_api_instrumentation_scope_integration" {
  // 测试跨API的InstrumentationScope使用
  let scope = common::InstrumentationScope::{
    name: "test-scope",
    version: Some("1.2.3"),
    schema_url: Some("http://example.com/test-schema")
  }
  
  // 创建带有scope的日志记录
  let log_record = logs::LogRecord::builder()
    .timestamp(3000000L)
    .severity(logs::Debug)
    .body("Debug message with scope")
    .with_attribute("scope.name", common::AttributeValue::string(scope.name))
    .with_attribute("scope.version", common::AttributeValue::string(scope.version.unwrap()))
    .build()
  
  // 验证scope信息
  @assert.eq(scope.name, "test-scope")
  @assert.eq(scope.version.unwrap(), "1.2.3")
  @assert.eq(scope.schema_url.unwrap(), "http://example.com/test-schema")
  @assert.eq(log_record.attributes.length(), 2)
}

test "cross_api_attribute_value_consistency" {
  // 测试跨API的属性值类型一致性
  let string_attr = common::AttributeValue::string("test-value")
  let int_attr = common::AttributeValue::int(42L)
  let float_attr = common::AttributeValue::float(3.14)
  let bool_attr = common::AttributeValue::bool(true)
  let array_string_attr = common::AttributeValue::array_string(["a", "b", "c"])
  let array_int_attr = common::AttributeValue::array_int([1L, 2L, 3L])
  
  // 在Trace中使用属性
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("attr-test-tracer")
  let (_, span) = tracer.start_span(
    context::Context::current(),
    "attribute-test-span",
    trace::Internal,
    [
      ("string.attr", string_attr),
      ("int.attr", int_attr),
      ("float.attr", float_attr),
      ("bool.attr", bool_attr),
      ("array.string.attr", array_string_attr),
      ("array.int.attr", array_int_attr)
    ]
  )
  
  // 在Metrics中使用属性
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("attr-test-meter")
  let counter = meter.create_counter("attr-test-counter")
  counter.add(1L, [
    ("string.attr", string_attr),
    ("int.attr", int_attr),
    ("float.attr", float_attr),
    ("bool.attr", bool_attr)
  ])
  
  // 在Logs中使用属性
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("attr-test-logger")
  logger.info("Attribute test log", [
    ("string.attr", string_attr),
    ("int.attr", int_attr),
    ("float.attr", float_attr),
    ("bool.attr", bool_attr)
  ])
  
  // 验证属性数量
  @assert.eq(span.attributes.length(), 6)
  
  // 验证属性值类型
  match span.attributes[0].1 {
    common::StringValue(v) => @assert.eq(v, "test-value")
    _ => @assert.fail("Expected StringValue")
  }
  
  match span.attributes[1].1 {
    common::IntValue(v) => @assert.eq(v, 42L)
    _ => @assert.fail("Expected IntValue")
  }
  
  match span.attributes[2].1 {
    common::FloatValue(v) => @assert.eq(v, 3.14)
    _ => @assert.fail("Expected FloatValue")
  }
  
  match span.attributes[3].1 {
    common::BoolValue(v) => @assert.eq(v, true)
    _ => @assert.fail("Expected BoolValue")
  }
}