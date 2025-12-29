// Azimuth Telemetry - 综合API测试用例
// 覆盖各种API组件的核心功能

test "attribute_value_type_conversions" {
  // 测试AttributeValue类型转换功能
  
  // 字符串值测试
  let string_attr = AttributeValue::string("test_value")
  match string_attr {
    StringValue(_) => assert_eq(true, true)
    _ => @test.fail("Expected StringValue")
  }
  
  // 整数值测试
  let int_attr = AttributeValue::int(42L)
  match int_attr {
    IntValue(_) => assert_eq(true, true)
    _ => @test.fail("Expected IntValue")
  }
  
  // 浮点值测试
  let float_attr = AttributeValue::float(3.14)
  match float_attr {
    FloatValue(_) => assert_eq(true, true)
    _ => @test.fail("Expected FloatValue")
  }
  
  // 布尔值测试
  let bool_attr = AttributeValue::bool(true)
  match bool_attr {
    BoolValue(_) => assert_eq(true, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  // 数组值测试
  let string_array = ["value1", "value2", "value3"]
  let array_attr = AttributeValue::array_string(string_array)
  match array_attr {
    ArrayStringValue(_) => assert_eq(true, true)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  // 属性对创建
  let attributes = [
    ("string_key", string_attr),
    ("int_key", int_attr),
    ("float_key", float_attr),
    ("bool_key", bool_attr),
    ("array_key", array_attr)
  ]
  
  assert_eq(attributes.length(), 5)
  assert_eq(attributes[0].0, "string_key")
  assert_eq(attributes[1].0, "int_key")
}

test "resource_structure_and_defaults" {
  // 测试Resource结构体和默认值
  
  // 创建默认资源
  let resource = Resource::default("test-service")
  
  // 验证基本字段
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.service_version, None)
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
  
  // 测试带属性的默认资源
  let attrs : Attributes = [
    ("env", AttributeValue::string("production")),
    ("version", AttributeValue::string("1.2.3"))
  ]
  
  assert_eq(attrs.length(), 2)
  match attrs[0].1 {
    StringValue(s) => assert_eq(s, "production")
    _ => @test.fail("Expected StringValue")
  }
}

test "span_kinds_and_status_codes" {
  // 测试SpanKind和StatusCode枚举
  
  // 测试SpanKind枚举值
  let internal_span = Internal
  let server_span = Server
  let client_span = Client
  let producer_span = Producer
  let consumer_span = Consumer
  
  // 创建不同类型的span上下文
  let trace_id = Array::make(16, 1_byte)
  let span_id = Array::make(8, 2_byte)
  
  let span_context = SpanContext::{
    trace_id,
    span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 测试不同类型的span
  let span1 = Span::{ name: "internal_op", context: span_context, kind: internal_span, parent_span_id: None, start_time_unix_nanos: 1000L, end_time_unix_nanos: Some(2000L), status: Ok, status_description: None, attributes: [], events: [], links: [] }
  let span2 = Span::{ name: "server_request", context: span_context, kind: server_span, parent_span_id: None, start_time_unix_nanos: 1000L, end_time_unix_nanos: Some(2000L), status: Ok, status_description: None, attributes: [], events: [], links: [] }
  let span3 = Span::{ name: "client_call", context: span_context, kind: client_span, parent_span_id: Some(span_id), start_time_unix_nanos: 1000L, end_time_unix_nanos: Some(2000L), status: Error, status_description: Some("Connection timeout"), attributes: [], events: [], links: [] }
  
  assert_eq(span1.kind, Internal)
  assert_eq(span2.kind, Server)
  assert_eq(span3.kind, Client)
  assert_eq(span3.status, Error)
  match span3.status_description {
    Some(desc) => assert_eq(desc, "Connection timeout")
    None => @test.fail("Expected Some(status_description)")
  }
}

test "instrument_types_and_metrics" {
  // 测试InstrumentType枚举和指标
  
  // 测试所有仪器类型
  let counter_type = Counter
  let histogram_type = Histogram
  let up_down_counter_type = UpDownCounter
  let gauge_type = Gauge
  let observable_counter_type = ObservableCounter
  let observable_gauge_type = ObservableGauge
  let observable_up_down_counter_type = ObservableUpDownCounter
  
  // 创建测量数据
  let attrs1 = [("method", AttributeValue::string("GET"))]
  let attrs2 = [("method", AttributeValue::string("POST"))]
  let attrs3 = [("method", AttributeValue::string("PUT"))]
  
  let measurements = [
    Measurement::{ value: 100.0, attributes: attrs1 },
    Measurement::{ value: 200.0, attributes: attrs2 },
    Measurement::{ value: 150.0, attributes: attrs3 }
  ]
  
  assert_eq(measurements.length(), 3)
  assert_eq(measurements[0].value, 100.0)
  match measurements[1].attributes[0].1 {
    StringValue(s) => assert_eq(s, "POST")
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试仪器类型数组
  let instrument_types = [counter_type, histogram_type, up_down_counter_type, gauge_type]
  assert_eq(instrument_types.length(), 4)
}

test "severity_numbers_and_logging" {
  // 测试SeverityNumber枚举和日志记录
  
  // 测试所有严重性级别
  let trace_severity = Trace
  let debug_severity = Debug
  let info_severity = Info
  let warn_severity = Warn
  let error_severity = Error
  let fatal_severity = Fatal
  
  // 创建不同严重性级别的日志记录
  let log_attrs = [("error_code", AttributeValue::int(500L))]
  
  let log_record1 = LogRecord::{ timestamp_unix_nanos: 1000L, observed_timestamp_unix_nanos: Some(1001L), severity_number: trace_severity, severity_text: Some("TRACE"), body: Some("Detailed trace information"), attributes: [], trace_id: None, span_id: None, trace_flags: None, resource: None, instrumentation_scope: None }
  let log_record2 = LogRecord::{ timestamp_unix_nanos: 2000L, observed_timestamp_unix_nanos: Some(2001L), severity_number: info_severity, severity_text: Some("INFO"), body: Some("Application started"), attributes: [], trace_id: None, span_id: None, trace_flags: None, resource: None, instrumentation_scope: None }
  let log_record3 = LogRecord::{ timestamp_unix_nanos: 3000L, observed_timestamp_unix_nanos: Some(3001L), severity_number: error_severity, severity_text: Some("ERROR"), body: Some("Database connection failed"), attributes: log_attrs, trace_id: None, span_id: None, trace_flags: None, resource: None, instrumentation_scope: None }
  
  assert_eq(log_record1.severity_number, Trace)
  match log_record2.severity_text {
    Some(text) => assert_eq(text, "INFO")
    None => @test.fail("Expected Some(severity_text)")
  }
  assert_eq(log_record3.severity_number, Error)
  assert_eq(log_record3.attributes.length(), 1)
}

test "context_and_baggage_operations" {
  // 测试Context和Baggage操作
  
  // 创建空上下文
  let empty_ctx = Context::empty()
  assert_eq(empty_ctx.values.length(), 0)
  
  // 创建上下文键
  let user_id_key = create_key("user_id")
  let request_id_key = create_key("request_id")
  
  // 添加值到上下文
  let ctx_with_user = empty_ctx.with_value(user_id_key, "user123")
  let ctx_with_both = ctx_with_user.with_value(request_id_key, "req456")
  
  // 验证上下文值
  match ctx_with_both.get(user_id_key) {
    Some(value) => assert_eq(value, "user123")
    None => @test.fail("Expected Some(value) for user_id")
  }
  match ctx_with_both.get(request_id_key) {
    Some(value) => assert_eq(value, "req456")
    None => @test.fail("Expected Some(value) for request_id")
  }
  assert_eq(ctx_with_both.values.length(), 2)
  
  // 测试Baggage操作
  let empty_baggage = Baggage::empty()
  assert_eq(empty_baggage.entries.length(), 0)
  
  let baggage_with_entry = empty_baggage.with_entry("correlation_id", "corr789")
  let baggage_with_multiple = baggage_with_entry.with_entry("session_id", "sess101")
  
  // 验证baggage条目
  match baggage_with_multiple.get("correlation_id") {
    Some(value) => assert_eq(value, "corr789")
    None => @test.fail("Expected Some(value) for correlation_id")
  }
  match baggage_with_multiple.get("session_id") {
    Some(value) => assert_eq(value, "sess101")
    None => @test.fail("Expected Some(value) for session_id")
  }
  assert_eq(baggage_with_multiple.entries.length(), 2)
}

test "text_map_carrier_implementation" {
  // 测试TextMapCarrier实现
  
  // 创建MapCarrier
  let carrier_data = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"),
    ("baggage", "key1=value1,key2=value2")
  ]
  
  let map_carrier = MapCarrier::from_map(carrier_data)
  
  // 测试get操作
  match map_carrier.get("traceparent") {
    Some(value) => assert_eq(value, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    None => @test.fail("Expected Some(value) for traceparent")
  }
  match map_carrier.get("baggage") {
    Some(value) => assert_eq(value, "key1=value1,key2=value2")
    None => @test.fail("Expected Some(value) for baggage")
  }
  match map_carrier.get("nonexistent") {
    Some(_) => @test.fail("Expected None for nonexistent key")
    None => assert_eq(true, true)
  }
  
  // 测试keys操作
  let keys = map_carrier.keys()
  assert_eq(keys.length(), 3)
  
  // 手动检查keys是否包含预期值
  let mut found_traceparent = false
  let mut found_tracestate = false
  let mut found_baggage = false
  let mut i = 0
  while i < keys.length() {
    if keys[i] == "traceparent" { found_traceparent = true }
    if keys[i] == "tracestate" { found_tracestate = true }
    if keys[i] == "baggage" { found_baggage = true }
    i = i + 1
  }
  assert_eq(found_traceparent, true)
  assert_eq(found_tracestate, true)
  assert_eq(found_baggage, true)
  
  // 测试to_map操作
  let extracted_data = map_carrier.to_map()
  assert_eq(extracted_data.length(), 3)
  assert_eq(extracted_data[0].0, "traceparent")
}

test "log_record_builder_pattern" {
  // 测试LogRecordBuilder构建器模式
  
  // 使用构建器创建日志记录
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)  // 2022-01-01 00:00:00 UTC
    .severity(Error)
    .body("Payment processing failed")
    .with_attribute("error_code", AttributeValue::int(400L))
    .with_attribute("payment_id", AttributeValue::string("pay123"))
    .build()
  
  // 验证构建的日志记录
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(log_record.severity_number, Error)
  match log_record.body {
    Some(body) => assert_eq(body, "Payment processing failed")
    None => @test.fail("Expected Some(body)")
  }
  assert_eq(log_record.attributes.length(), 2)
  assert_eq(log_record.attributes[0].0, "error_code")
  match log_record.attributes[1].1 {
    StringValue(s) => assert_eq(s, "pay123")
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试默认值
  let minimal_log = LogRecord::builder().build()
  assert_eq(minimal_log.timestamp_unix_nanos, 0L)
  assert_eq(minimal_log.severity_number, Info)
  match minimal_log.body {
    Some(_) => @test.fail("Expected None for body")
    None => assert_eq(true, true)
  }
  assert_eq(minimal_log.attributes.length(), 0)
}

test "cross_module_integration" {
  // 测试跨模块集成
  
  // 创建资源
  let resource = Resource::default("integration-test")
  
  // 创建上下文和baggage
  let user_key = create_key("user_id")
  let ctx = Context::empty()
    .with_value(user_key, "user456")
  let baggage = Baggage::empty()
    .with_entry("trace_id", "trace789")
  
  // 创建span上下文
  let span_context = SpanContext::{
    trace_id: Array::make(16, 10_byte),
    span_id: Array::make(8, 20_byte),
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 创建span属性
  let span_attrs = [
    ("http.method", AttributeValue::string("GET")),
    ("http.status_code", AttributeValue::int(200L))
  ]
  
  // 创建span
  let span = Span::{
    name: "integration_span",
    context: span_context,
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(2000L),
    status: Ok,
    status_description: None,
    attributes: span_attrs,
    events: [],
    links: []
  }
  
  // 创建日志记录属性
  let log_attrs = [("duration_ms", AttributeValue::float(500.0))]
  
  // 创建日志记录
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1500L,
    observed_timestamp_unix_nanos: Some(1501L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Request processed successfully"),
    attributes: log_attrs,
    trace_id: Some(span_context.trace_id),
    span_id: Some(span_context.span_id),
    trace_flags: Some(span_context.trace_flags),
    resource: Some(resource),
    instrumentation_scope: None
  }
  
  // 验证集成数据
  assert_eq(resource.service_name, "integration-test")
  match ctx.get(user_key) {
    Some(value) => assert_eq(value, "user456")
    None => @test.fail("Expected Some(value) for user_id")
  }
  match baggage.get("trace_id") {
    Some(value) => assert_eq(value, "trace789")
    None => @test.fail("Expected Some(value) for trace_id")
  }
  assert_eq(span.name, "integration_span")
  assert_eq(span.attributes.length(), 2)
  match log_record.body {
    Some(body) => assert_eq(body, "Request processed successfully")
    None => @test.fail("Expected Some(body)")
  }
  match log_record.resource {
    Some(res) => assert_eq(res.service_name, "integration-test")
    None => @test.fail("Expected Some(resource)")
  }
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  
  // 测试空字符串和特殊字符
  let empty_string_attr = AttributeValue::string("")
  let special_chars_attr = AttributeValue::string("特殊字符&symbols=@#$")
  
  match empty_string_attr {
    StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Expected StringValue")
  }
  
  match special_chars_attr {
    StringValue(s) => assert_eq(s, "特殊字符&symbols=@#$")
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试极值
  let max_int_attr = AttributeValue::int(9223372036854775807L)  // Int64最大值
  let min_int_attr = AttributeValue::int(-9223372036854775808L)  // Int64最小值
  let inf_float_attr = AttributeValue::float(1.0 / 0.0)  // 无穷大
  let neg_inf_float_attr = AttributeValue::float(-1.0 / 0.0)  // 负无穷大
  
  // 测试空数组
  let empty_string_array = AttributeValue::array_string([])
  let empty_int_array = AttributeValue::array_int([])
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  // 测试包含None的Resource
  let resource_with_none = Resource::default("test")
  
  assert_eq(resource_with_none.service_version, None)
  
  // 测试包含None的LogRecord
  let log_record_with_none = LogRecord::{
    timestamp_unix_nanos: 1000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Warn,
    severity_text: None,
    body: None,
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证边界情况处理
  match log_record_with_none.body {
    Some(_) => @test.fail("Expected None for body")
    None => assert_eq(true, true)
  }
  match log_record_with_none.observed_timestamp_unix_nanos {
    Some(_) => @test.fail("Expected None for observed_timestamp")
    None => assert_eq(true, true)
  }
  match log_record_with_none.resource {
    Some(_) => @test.fail("Expected None for resource")
    None => assert_eq(true, true)
  }
  
  // 测试空上下文中的键查找
  let empty_ctx = Context::empty()
  let non_existent_key = create_key("non_existent")
  match empty_ctx.get(non_existent_key) {
    Some(_) => @test.fail("Expected None for non_existent key")
    None => assert_eq(true, true)
  }
  
  // 测试空baggage中的条目查找
  let empty_baggage = Baggage::empty()
  match empty_baggage.get("non_existent") {
    Some(_) => @test.fail("Expected None for non_existent entry")
    None => assert_eq(true, true)
  }
}