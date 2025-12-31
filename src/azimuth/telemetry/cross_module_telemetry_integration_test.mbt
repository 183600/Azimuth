// 跨模块遥测数据集成测试
// 测试trace、metrics、logs三个模块的数据集成和关联性

test "cross_module_telemetry_correlation_integration" {
  // 创建共享的资源
  let resource = common::Resource::default("integration-test-service")
  
  // 创建关联的trace context
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { (i % 256).to_byte() })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { (i % 256).to_byte() })
  
  let span_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 1_byte,
    trace_state: "test=1"
  }
  
  // 创建带有trace关联的log记录
  let log_record = logs::LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000000L),
    severity_number: logs::Info,
    severity_text: Some("INFO"),
    body: Some("Integration test log entry"),
    attributes: [("test.id", common::AttributeValue::string("integration-001"))],
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(1_byte),
    resource: Some(resource),
    instrumentation_scope: Some(common::InstrumentationScope::{
      name: "integration-test",
      version: Some("1.0.0"),
      schema_url: Some("https://example.com/schema")
    })
  }
  
  // 创建metric measurement，带有相同的trace context属性
  let measurement = metrics::Measurement::{
    value: 42.5,
    attributes: [
      ("test.id", common::AttributeValue::string("integration-001")),
      ("trace.id", common::AttributeValue::array_string([trace_id.map(fn(b) { b.to_string() }).join("")]))
    ]
  }
  
  // 验证数据关联性
  @assertion.assert_eq(log_record.trace_id?, trace_id)
  @assertion.assert_eq(log_record.span_id?, span_id)
  @assertion.assert_eq(measurement.attributes[0].0, "test.id")
  @assertion.assert_eq(measurement.attributes[0].1, common::AttributeValue::string("integration-001"))
  
  // 验证资源信息一致性
  @assertion.assert_eq(log_record.resource?.service_name, "integration-test-service")
  @assertion.assert_eq(log_record.resource?.telemetry_sdk_name, "azimuth")
}

test "cross_module_telemetry_data_flow_integration" {
  // 模拟完整的遥测数据流：从trace开始，关联metrics和logs
  
  // 1. 创建root span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("integration-test-tracer", "1.0.0")
  let (ctx, root_span) = tracer.start_span(
    @context.get_context(),
    "integration-operation",
    trace::Server,
    [("operation.type", common::AttributeValue::string("integration"))],
    1640995200000000000L
  )
  
  // 2. 在span上下文中记录metrics
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("integration-test-meter", "1.0.0")
  let counter = meter.create_counter("integration.operations", "count", "Number of integration operations")
  
  counter.add(1L, [
    ("operation.name", common::AttributeValue::string(root_span.name)),
    ("span.kind", common::AttributeValue::string("Server"))
  ])
  
  // 3. 在span上下文中记录log
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("integration-test-logger", "1.0.0")
  
  let correlated_log = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("Integration operation completed")
    .with_attribute("operation.name", common::AttributeValue::string(root_span.name))
    .with_attribute("span.trace_id", common::AttributeValue::array_string([
      root_span.context.trace_id.map(fn(b) { b.to_string() }).join("")
    ]))
    .build()
  
  logger.emit(correlated_log)
  
  // 4. 验证数据流的完整性
  @assertion.assert_eq(root_span.name, "integration-operation")
  @assertion.assert_eq(root_span.kind, trace::Server)
  @assertion.assert_eq(root_span.attributes.length(), 1)
  @assertion.assert_eq(root_span.attributes[0].0, "operation.type")
  
  @assertion.assert_eq(correlated_log.body?, "Integration operation completed")
  @assertion.assert_eq(correlated_log.severity_number, logs::Info)
  @assertion.assert_eq(correlated_log.attributes.length(), 2)
}

test "cross_module_telemetry_attribute_consistency" {
  // 测试跨模块的属性类型一致性和转换
  
  let test_attributes = [
    ("string.attr", common::AttributeValue::string("test-value")),
    ("int.attr", common::AttributeValue::int(42L)),
    ("float.attr", common::AttributeValue::float(3.14)),
    ("bool.attr", common::AttributeValue::bool(true)),
    ("string.array.attr", common::AttributeValue::array_string(["a", "b", "c"])),
    ("int.array.attr", common::AttributeValue::array_int([1L, 2L, 3L])),
    ("float.array.attr", common::AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool.array.attr", common::AttributeValue::array_bool([true, false, true]))
  ]
  
  // 在span中使用这些属性
  let span_context = trace::SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte }),
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let test_span = trace::Span::{
    name: "attribute-test",
    context: span_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200000000000L),
    status: trace::Ok,
    status_description: None,
    attributes: test_attributes,
    events: [],
    links: []
  }
  
  // 在log记录中使用相同的属性
  let test_log = logs::LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: logs::Info,
    severity_text: Some("INFO"),
    body: Some("Attribute consistency test"),
    attributes: test_attributes,
    trace_id: Some(span_context.trace_id),
    span_id: Some(span_context.span_id),
    trace_flags: Some(1_byte),
    resource: None,
    instrumentation_scope: None
  }
  
  // 在measurement中使用相同的属性
  let test_measurement = metrics::Measurement::{
    value: 100.0,
    attributes: test_attributes
  }
  
  // 验证属性在所有模块中的一致性
  @assertion.assert_eq(test_span.attributes.length(), test_attributes.length())
  @assertion.assert_eq(test_log.attributes.length(), test_attributes.length())
  @assertion.assert_eq(test_measurement.attributes.length(), test_attributes.length())
  
  // 验证具体属性值
  @assertion.assert_eq(test_span.attributes[0].1, common::AttributeValue::string("test-value"))
  @assertion.assert_eq(test_log.attributes[1].1, common::AttributeValue::int(42L))
  @assertion.assert_eq(test_measurement.attributes[2].1, common::AttributeValue::float(3.14))
}

test "cross_module_telemetry_temporal_correlation" {
  // 测试跨模块的时间关联性
  
  let base_timestamp = 1640995200000000000L  // 2022-01-01 00:00:00 UTC
  
  // 创建时间序列的遥测数据
  let span_start = base_timestamp
  let span_end = base_timestamp + 1000000L  // 1ms后
  
  let span_context = trace::SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 2_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 2_byte }),
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let time_correlated_span = trace::Span::{
    name: "temporal-test",
    context: span_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: span_start,
    end_time_unix_nanos: Some(span_end),
    status: trace::Ok,
    status_description: None,
    attributes: [("duration.ms", common::AttributeValue::int(1L))],
    events: [],
    links: []
  }
  
  // 在span执行期间记录metric
  let metric_timestamp = span_start + 500000L  // 0.5ms后
  let duration_measurement = metrics::Measurement::{
    value: 1.0,
    attributes: [
      ("operation.duration", common::AttributeValue::float(1.0)),
      ("timestamp", common::AttributeValue::int(metric_timestamp))
    ]
  }
  
  // 在span结束时记录log
  let completion_log = logs::LogRecord::{
    timestamp_unix_nanos: span_end,
    observed_timestamp_unix_nanos: None,
    severity_number: logs::Info,
    severity_text: Some("INFO"),
    body: Some("Operation completed successfully"),
    attributes: [
      ("duration.ms", common::AttributeValue::int(1L)),
      ("completion.reason", common::AttributeValue::string("success"))
    ],
    trace_id: Some(span_context.trace_id),
    span_id: Some(span_context.span_id),
    trace_flags: Some(1_byte),
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证时间关联性
  @assertion.assert_eq(time_correlated_span.start_time_unix_nanos, span_start)
  @assertion.assert_eq(time_correlated_span.end_time_unix_nanos?, span_end)
  @assertion.assert_eq(completion_log.timestamp_unix_nanos, span_end)
  @assertion.assert_eq(duration_measurement.attributes[1].1, common::AttributeValue::int(metric_timestamp))
  
  // 验证时间顺序逻辑
  @assertion.assert_true(span_start <= metric_timestamp)
  @assertion.assert_true(metric_timestamp <= span_end)
  @assertion.assert_true(span_end == completion_log.timestamp_unix_nanos)
}