// API核心功能单元测试
// 测试Trace、Metrics、Logs API的基本功能

use azimuth.telemetry.api.common
use azimuth.telemetry.api.trace
use azimuth.telemetry.api.metrics
use azimuth.telemetry.api.logs

test "api_trace_span_creation" {
  // 测试Span创建功能
  
  let span_context = trace::SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { i.to_byte() }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { i.to_byte() }),
    trace_flags: 1_byte,
    trace_state: "test-state"
  }
  
  // 验证SpanContext
  assert_eq(span_context.trace_id.length(), 16)
  assert_eq(span_context.span_id.length(), 8)
  assert_eq(span_context.trace_flags, 1_byte)
  assert_eq(span_context.trace_state, "test-state")
  
  let span = trace::Span::{
    name: "test-span",
    context: span_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200L,
    end_time_unix_nanos: Some(1640995300L),
    status: trace::Ok,
    status_description: Some("Success"),
    attributes: [("test.key", common::AttributeValue::string("test.value"))],
    events: [],
    links: []
  }
  
  // 验证Span
  assert_eq(span.name, "test-span")
  assert_eq(span.kind, trace::Internal)
  assert_eq(span.status, trace::Ok)
  assert_eq(span.status_description.unwrap(), "Success")
  assert_eq(span.attributes.length(), 1)
  assert_eq(span.attributes[0].0, "test.key")
  assert_eq(span.attributes[0].1, common::AttributeValue::string("test.value"))
}

test "api_metrics_instrument_creation" {
  // 测试指标创建功能
  
  let noop_tracer_provider = trace::NoopTracerProvider::{}
  let tracer = noop_tracer_provider.get_tracer("test-tracer", "1.0.0")
  
  let noop_meter_provider = metrics::NoopMeterProvider::{}
  let meter = noop_meter_provider.get_meter("test-meter", "1.0.0", "http://example.com/schema")
  
  // 创建各种指标
  let counter = meter.create_counter("test_counter", "count", "Test counter metric")
  let histogram = meter.create_histogram("test_histogram", "ms", "Test histogram metric")
  let up_down_counter = meter.create_up_down_counter("test_up_down", "count", "Test up-down counter")
  let gauge = meter.create_gauge("test_gauge", "percent", "Test gauge metric")
  
  // 验证指标创建（不会抛出异常即为成功）
  counter.add(10L, [("tag.key", common::AttributeValue::string("tag.value"))])
  histogram.record(100.5, [("operation.type", common::AttributeValue::string("read"))])
  up_down_counter.add(-5L, [("status", common::AttributeValue::string("failure"))])
  gauge.record(75.0, [("component", common::AttributeValue::string("cache"))])
  
  assert_eq(true, true) // 如果没有异常，说明创建成功
}

test "api_logs_record_creation" {
  // 测试日志记录创建功能
  
  let noop_logger_provider = logs::NoopLoggerProvider::{}
  let logger = noop_logger_provider.get_logger("test-logger", "1.0.0", "http://example.com/schema")
  
  // 创建日志记录
  let log_record = logs::LogRecord::{
    timestamp_unix_nanos: 1640995200L,
    observed_timestamp_unix_nanos: Some(1640995201L),
    severity_number: logs::Info,
    severity_text: Some("INFO"),
    body: Some("Test log message"),
    attributes: [
      ("service.name", common::AttributeValue::string("test-service")),
      ("user.id", common::AttributeValue::int(12345L))
    ],
    trace_id: Some([for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })),
    span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 2_byte })),
    trace_flags: Some(1_byte),
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证日志记录
  assert_eq(log_record.severity_number, logs::Info)
  assert_eq(log_record.severity_text.unwrap(), "INFO")
  assert_eq(log_record.body.unwrap(), "Test log message")
  assert_eq(log_record.attributes.length(), 2)
  assert_eq(log_record.attributes[0].0, "service.name")
  assert_eq(log_record.attributes[0].1, common::AttributeValue::string("test-service"))
  assert_eq(log_record.attributes[1].1, common::AttributeValue::int(12345L))
  
  // 测试日志记录发送
  logger.emit(log_record)
  logger.debug("Debug message", [("level", common::AttributeValue::string("debug"))])
  logger.info("Info message", [("level", common::AttributeValue::string("info"))])
  logger.warn("Warning message", [("level", common::AttributeValue::string("warn"))])
  logger.error("Error message", [("level", common::AttributeValue::string("error"))])
  logger.fatal("Fatal message", [("level", common::AttributeValue::string("fatal"))])
  
  assert_eq(true, true) // 如果没有异常，说明发送成功
}

test "api_common_attribute_values" {
  // 测试公共属性值类型
  
  // 测试各种属性值类型
  let string_attr = common::AttributeValue::string("test-string")
  let int_attr = common::AttributeValue::int(42L)
  let float_attr = common::AttributeValue::float(3.14159)
  let bool_attr = common::AttributeValue::bool(true)
  let array_string_attr = common::AttributeValue::array_string(["a", "b", "c"])
  let array_int_attr = common::AttributeValue::array_int([1L, 2L, 3L])
  let array_float_attr = common::AttributeValue::array_float([1.1, 2.2, 3.3])
  let array_bool_attr = common::AttributeValue::array_bool([true, false, true])
  
  // 验证属性值创建
  match string_attr {
    StringValue(s) => assert_eq(s, "test-string")
    _ => assert_eq(false, true)
  }
  
  match int_attr {
    IntValue(i) => assert_eq(i, 42L)
    _ => assert_eq(false, true)
  }
  
  match float_attr {
    FloatValue(f) => assert_eq(f, 3.14159)
    _ => assert_eq(false, true)
  }
  
  match bool_attr {
    BoolValue(b) => assert_eq(b, true)
    _ => assert_eq(false, true)
  }
  
  match array_string_attr {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[2], "c")
    }
    _ => assert_eq(false, true)
  }
}

test "api_common_resource_creation" {
  // 测试资源创建功能
  
  let resource = common::Resource::default("test-service")
  
  // 验证默认资源
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.service_version, None)
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
  
  // 创建自定义资源
  let custom_resource = common::Resource::{
    service_name: "custom-service",
    service_version: Some("2.0.0"),
    telemetry_sdk_name: "custom-sdk",
    telemetry_sdk_version: "1.5.0",
    attributes: [
      ("environment", common::AttributeValue::string("production")),
      ("region", common::AttributeValue::string("us-west-2"))
    ]
  }
  
  // 验证自定义资源
  assert_eq(custom_resource.service_name, "custom-service")
  assert_eq(custom_resource.service_version.unwrap(), "2.0.0")
  assert_eq(custom_resource.telemetry_sdk_name, "custom-sdk")
  assert_eq(custom_resource.telemetry_sdk_version, "1.5.0")
  assert_eq(custom_resource.attributes.length(), 2)
  assert_eq(custom_resource.attributes[0].0, "environment")
  assert_eq(custom_resource.attributes[0].1, common::AttributeValue::string("production"))
}

test "api_common_instrumentation_scope" {
  // 测试工具范围创建功能
  
  let instrumentation_scope = common::InstrumentationScope::{
    name: "test-instrumentation",
    version: Some("1.0.0"),
    schema_url: Some("http://example.com/schema")
  }
  
  // 验证工具范围
  assert_eq(instrumentation_scope.name, "test-instrumentation")
  assert_eq(instrumentation_scope.version.unwrap(), "1.0.0")
  assert_eq(instrumentation_scope.schema_url.unwrap(), "http://example.com/schema")
  
  // 创建最小工具范围
  let minimal_scope = common::InstrumentationScope::{
    name: "minimal-scope",
    version: None,
    schema_url: None
  }
  
  // 验证最小工具范围
  assert_eq(minimal_scope.name, "minimal-scope")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
}

test "api_trace_span_kinds" {
  // 测试Span种类
  
  let internal_span_kind = trace::Internal
  let server_span_kind = trace::Server
  let client_span_kind = trace::Client
  let producer_span_kind = trace::Producer
  let consumer_span_kind = trace::Consumer
  
  // 验证Span种类创建
  assert_eq(internal_span_kind, trace::Internal)
  assert_eq(server_span_kind, trace::Server)
  assert_eq(client_span_kind, trace::Client)
  assert_eq(producer_span_kind, trace::Producer)
  assert_eq(consumer_span_kind, trace::Consumer)
  
  // 创建不同种类的Span
  let spans = [
    trace::Span::{ name: "internal-op", context: trace::SpanContext::{ trace_id: [], span_id: [], trace_flags: 0_byte, trace_state: "" }, kind: trace::Internal, parent_span_id: None, start_time_unix_nanos: 0L, end_time_unix_nanos: None, status: trace::Unset, status_description: None, attributes: [], events: [], links: [] },
    trace::Span::{ name: "server-op", context: trace::SpanContext::{ trace_id: [], span_id: [], trace_flags: 0_byte, trace_state: "" }, kind: trace::Server, parent_span_id: None, start_time_unix_nanos: 0L, end_time_unix_nanos: None, status: trace::Unset, status_description: None, attributes: [], events: [], links: [] },
    trace::Span::{ name: "client-op", context: trace::SpanContext::{ trace_id: [], span_id: [], trace_flags: 0_byte, trace_state: "" }, kind: trace::Client, parent_span_id: None, start_time_unix_nanos: 0L, end_time_unix_nanos: None, status: trace::Unset, status_description: None, attributes: [], events: [], links: [] }
  ]
  
  // 验证不同种类的Span
  assert_eq(spans.length(), 3)
  assert_eq(spans[0].name, "internal-op")
  assert_eq(spans[0].kind, trace::Internal)
  assert_eq(spans[1].name, "server-op")
  assert_eq(spans[1].kind, trace::Server)
  assert_eq(spans[2].name, "client-op")
  assert_eq(spans[2].kind, trace::Client)
}

test "api_trace_status_codes" {
  // 测试状态码
  
  let unset_status = trace::Unset
  let ok_status = trace::Ok
  let error_status = trace::Error
  
  // 验证状态码
  assert_eq(unset_status, trace::Unset)
  assert_eq(ok_status, trace::Ok)
  assert_eq(error_status, trace::Error)
  
  // 创建不同状态的Span
  let spans = [
    trace::Span::{ name: "unset-span", context: trace::SpanContext::{ trace_id: [], span_id: [], trace_flags: 0_byte, trace_state: "" }, kind: trace::Internal, parent_span_id: None, start_time_unix_nanos: 0L, end_time_unix_nanos: None, status: trace::Unset, status_description: None, attributes: [], events: [], links: [] },
    trace::Span::{ name: "ok-span", context: trace::SpanContext::{ trace_id: [], span_id: [], trace_flags: 0_byte, trace_state: "" }, kind: trace::Internal, parent_span_id: None, start_time_unix_nanos: 0L, end_time_unix_nanos: None, status: trace::Ok, status_description: Some("Success"), attributes: [], events: [], links: [] },
    trace::Span::{ name: "error-span", context: trace::SpanContext::{ trace_id: [], span_id: [], trace_flags: 0_byte, trace_state: "" }, kind: trace::Internal, parent_span_id: None, start_time_unix_nanos: 0L, end_time_unix_nanos: None, status: trace::Error, status_description: Some("Failed"), attributes: [], events: [], links: [] }
  ]
  
  // 验证不同状态的Span
  assert_eq(spans.length(), 3)
  assert_eq(spans[0].status, trace::Unset)
  assert_eq(spans[1].status, trace::Ok)
  assert_eq(spans[1].status_description.unwrap(), "Success")
  assert_eq(spans[2].status, trace::Error)
  assert_eq(spans[2].status_description.unwrap(), "Failed")
}

test "api_logs_severity_levels" {
  // 测试日志严重性级别
  
  let trace_severity = logs::Trace
  let debug_severity = logs::Debug
  let info_severity = logs::Info
  let warn_severity = logs::Warn
  let error_severity = logs::Error
  let fatal_severity = logs::Fatal
  
  // 验证严重性级别
  assert_eq(trace_severity, logs::Trace)
  assert_eq(debug_severity, logs::Debug)
  assert_eq(info_severity, logs::Info)
  assert_eq(warn_severity, logs::Warn)
  assert_eq(error_severity, logs::Error)
  assert_eq(fatal_severity, logs::Fatal)
  
  // 创建不同严重性级别的日志记录
  let log_records = [
    logs::LogRecord::{ timestamp_unix_nanos: 0L, observed_timestamp_unix_nanos: None, severity_number: logs::Trace, severity_text: Some("TRACE"), body: Some("Trace message"), attributes: [], trace_id: None, span_id: None, trace_flags: None, resource: None, instrumentation_scope: None },
    logs::LogRecord::{ timestamp_unix_nanos: 0L, observed_timestamp_unix_nanos: None, severity_number: logs::Info, severity_text: Some("INFO"), body: Some("Info message"), attributes: [], trace_id: None, span_id: None, trace_flags: None, resource: None, instrumentation_scope: None },
    logs::LogRecord::{ timestamp_unix_nanos: 0L, observed_timestamp_unix_nanos: None, severity_number: logs::Error, severity_text: Some("ERROR"), body: Some("Error message"), attributes: [], trace_id: None, span_id: None, trace_flags: None, resource: None, instrumentation_scope: None }
  ]
  
  // 验证不同严重性级别的日志记录
  assert_eq(log_records.length(), 3)
  assert_eq(log_records[0].severity_number, logs::Trace)
  assert_eq(log_records[0].severity_text.unwrap(), "TRACE")
  assert_eq(log_records[1].severity_number, logs::Info)
  assert_eq(log_records[1].body.unwrap(), "Info message")
  assert_eq(log_records[2].severity_number, logs::Error)
  assert_eq(log_records[2].severity_text.unwrap(), "ERROR")
}

test "api_metrics_instrument_types" {
  // 测试指标工具类型
  
  let counter_type = metrics::Counter
  let histogram_type = metrics::Histogram
  let up_down_counter_type = metrics::UpDownCounter
  let gauge_type = metrics::Gauge
  let observable_counter_type = metrics::ObservableCounter
  let observable_gauge_type = metrics::ObservableGauge
  let observable_up_down_counter_type = metrics::ObservableUpDownCounter
  
  // 验证指标工具类型
  assert_eq(counter_type, metrics::Counter)
  assert_eq(histogram_type, metrics::Histogram)
  assert_eq(up_down_counter_type, metrics::UpDownCounter)
  assert_eq(gauge_type, metrics::Gauge)
  assert_eq(observable_counter_type, metrics::ObservableCounter)
  assert_eq(observable_gauge_type, metrics::ObservableGauge)
  assert_eq(observable_up_down_counter_type, metrics::ObservableUpDownCounter)
  
  // 创建测量数据
  let measurements = [
    metrics::Measurement::{ value: 100.0, attributes: [("operation", common::AttributeValue::string("read"))] },
    metrics::Measurement::{ value: 200.0, attributes: [("operation", common::AttributeValue::string("write"))] },
    metrics::Measurement::{ value: 50.0, attributes: [("operation", common::AttributeValue::string("delete"))] }
  ]
  
  // 验证测量数据
  assert_eq(measurements.length(), 3)
  assert_eq(measurements[0].value, 100.0)
  assert_eq(measurements[0].attributes[0].1, common::AttributeValue::string("read"))
  assert_eq(measurements[1].value, 200.0)
  assert_eq(measurements[2].attributes[0].1, common::AttributeValue::string("delete"))
}