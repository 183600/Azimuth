// 跨模块遥测数据集成测试
// 测试Trace、Metrics、Logs和Context模块之间的数据流动和一致性

test "trace_metrics_integration" {
  // 创建资源和上下文
  let resource = @azimuth.telemetry.api.common.Resource::default("integration_test_service")
  let context = @azimuth.telemetry.api.context.Context::empty()
  let trace_key = @azimuth.telemetry.api.context.create_key("trace_id")
  
  // 模拟生成trace信息
  let trace_id = "1234567890abcdef1234567890abcdef"
  let context_with_trace = context.with_value(trace_key, trace_id)
  
  // 创建Tracer和Span
  let tracer_provider = @azimuth.telemetry.api.trace.NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("integration_tracer", "1.0.0")
  let (ctx, span) = tracer.start_span(
    context_with_trace,
    "integration_operation",
    @azimuth.telemetry.api.trace.Server,
    [("operation_type", @azimuth.telemetry.api.common.AttributeValue::string("test"))]
  )
  
  // 创建Meter和指标
  let meter_provider = @azimuth.telemetry.api.metrics.NoopMeterProvider::{}
  let meter = meter_provider.get_meter("integration_meter", "1.0.0")
  let counter = meter.create_counter("operations_total", "count", "Total number of operations")
  let histogram = meter.create_histogram("operation_duration", "ms", "Operation duration in milliseconds")
  
  // 记录指标数据
  counter.add(1L, [("trace_id", @azimuth.telemetry.api.common.AttributeValue::string(trace_id))])
  histogram.record(150.5, [("operation", @azimuth.telemetry.api.common.AttributeValue::string(span.name))])
  
  // 创建Logger和日志
  let logger_provider = @azimuth.telemetry.api.logs.NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("integration_logger", "1.0.0")
  
  // 记录日志
  logger.info(
    "Operation completed successfully",
    [("trace_id", @azimuth.telemetry.api.common.AttributeValue::string(trace_id)), 
     ("span_name", @azimuth.telemetry.api.common.AttributeValue::string(span.name))]
  )
  
  // 验证数据一致性
  let retrieved_trace_id = ctx.get(trace_key)
  assert_eq(retrieved_trace_id.unwrap(), trace_id)
  assert_eq(span.name, "integration_operation")
  assert_eq(span.kind, trace::Server)
  assert_eq(span.attributes.length(), 1)
}

test "context_baggage_propagation" {
  // 创建带有baggage的上下文
  let baggage = @azimuth.telemetry.api.context.Baggage::empty()
    .with_entry("user_id", "12345")
    .with_entry("session_id", "abcdef")
    .with_entry("request_id", "req_789")
  
  let context = @azimuth.telemetry.api.context.Context::empty()
  let baggage_key = @azimuth.telemetry.api.context.create_key("baggage")
  let baggage_str = "user_id=12345,session_id=abcdef,request_id=req_789"
  let context_with_baggage = context.with_value(baggage_key, baggage_str)
  
  // 验证baggage内容
  assert_eq(baggage.get("user_id").unwrap(), "12345")
  assert_eq(baggage.get("session_id").unwrap(), "abcdef")
  assert_eq(baggage.get("request_id").unwrap(), "req_789")
  assert_eq(baggage.entries.length(), 3)
  
  // 验证上下文中的baggage
  let retrieved_baggage = context_with_baggage.get(baggage_key)
  assert_eq(retrieved_baggage.unwrap(), baggage_str)
}

test "log_trace_correlation" {
  // 创建模拟的trace和span ID
  let trace_id_bytes = [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0, 
                        0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0]
  let span_id_bytes = [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0]
  
  // 创建与trace关联的日志记录
  let log_record = logs::LogRecord::builder()
    .timestamp(1234567890000000L)
    .severity(logs::Info)
    .body("Processing user request")
    .with_attribute("user_id", common::AttributeValue::string("user123"))
    .with_attribute("action", common::AttributeValue::string("login"))
    .build()
  
  // 创建带有trace关联的日志记录
  let correlated_log = logs::LogRecord::{
    ..log_record,
    trace_id: trace_id_bytes,
    span_id: span_id_bytes,
    trace_flags: Some(1_byte)
  }
  
  // 验证日志记录的trace关联
  assert_eq(correlated_log.trace_id.unwrap().length(), 16)
  assert_eq(correlated_log.span_id.unwrap().length(), 8)
  assert_eq(correlated_log.trace_flags.unwrap(), 1_byte)
  assert_eq(correlated_log.body.unwrap(), "Processing user request")
  assert_eq(correlated_log.attributes.length(), 2)
}

test "multi_signal_attributes_consistency" {
  // 定义通用属性集
  let common_attributes = [
    ("service.name", common::AttributeValue::string("auth-service")),
    ("service.version", common::AttributeValue::string("2.1.0")),
    ("deployment.environment", common::AttributeValue::string("production")),
    ("host.name", common::AttributeValue::string("auth-prod-01"))
  ]
  
  // 在Span中使用这些属性
  let span_attributes = common_attributes + [
    ("span.type", common::AttributeValue::string("http")),
    ("http.method", common::AttributeValue::string("POST")),
    ("http.status_code", common::AttributeValue::int(200L))
  ]
  
  // 在Metric中使用这些属性
  let metric_attributes = common_attributes + [
    ("metric.name", common::AttributeValue::string("http_requests_total")),
    ("metric.type", common::AttributeValue::string("counter"))
  ]
  
  // 在Log中使用这些属性
  let log_attributes = common_attributes + [
    ("log.level", common::AttributeValue::string("INFO")),
    ("log.logger", common::AttributeValue::string("auth.logger"))
  ]
  
  // 验证属性一致性
  assert_eq(span_attributes.length(), 7)
  assert_eq(metric_attributes.length(), 6)
  assert_eq(log_attributes.length(), 6)
  
  // 验证通用属性在所有信号中存在
  let mut i = 0
  while i < common_attributes.length() {
    let (key, _) = common_attributes[i]
    assert_eq(span_attributes.any(fn(attr) { attr.0 == key }), true)
    assert_eq(metric_attributes.any(fn(attr) { attr.0 == key }), true)
    assert_eq(log_attributes.any(fn(attr) { attr.0 == key }), true)
    i = i + 1
  }
}