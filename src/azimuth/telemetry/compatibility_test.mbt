// 兼容性测试用例
// 测试telemetry系统的版本兼容性和向后兼容性

test "attribute_value_compatibility" {
  // 测试AttributeValue的版本兼容性
  
  // 测试所有支持的AttributeValue类型
  let string_attr = AttributeValue::string("test-string")
  let int_attr = AttributeValue::int(42L)
  let float_attr = AttributeValue::float(3.14159)
  let bool_attr = AttributeValue::bool(true)
  
  // 测试数组类型
  let string_array = ["a", "b", "c"]
  let int_array = [1L, 2L, 3L]
  let float_array = [1.1, 2.2, 3.3]
  let bool_array = [true, false, true]
  
  let array_string_attr = AttributeValue::array_string(string_array)
  let array_int_attr = AttributeValue::array_int(int_array)
  let array_float_attr = AttributeValue::array_float(float_array)
  let array_bool_attr = AttributeValue::array_bool(bool_array)
  
  // 验证所有类型的创建和基本操作
  match string_attr {
    StringValue(s) => assert_eq(s, "test-string")
    _ => @test.fail("Expected StringValue")
  }
  
  match int_attr {
    IntValue(i) => assert_eq(i, 42L)
    _ => @test.fail("Expected IntValue")
  }
  
  match float_attr {
    FloatValue(f) => assert_eq(f, 3.14159)
    _ => @test.fail("Expected FloatValue")
  }
  
  match bool_attr {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  match array_string_attr {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match array_int_attr {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1L)
      assert_eq(arr[1], 2L)
      assert_eq(arr[2], 3L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match array_float_attr {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1.1)
      assert_eq(arr[1], 2.2)
      assert_eq(arr[2], 3.3)
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match array_bool_attr {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
}

test "span_kind_status_compatibility" {
  // 测试SpanKind和StatusCode的兼容性
  
  // 测试所有SpanKind值
  let internal_kind = Internal
  let server_kind = Server
  let client_kind = Client
  let producer_kind = Producer
  let consumer_kind = Consumer
  
  // 验证SpanKind的可用性
  match internal_kind {
    Internal => assert_eq(true, true)
    _ => @test.fail("Expected Internal")
  }
  
  match server_kind {
    Server => assert_eq(true, true)
    _ => @test.fail("Expected Server")
  }
  
  match client_kind {
    Client => assert_eq(true, true)
    _ => @test.fail("Expected Client")
  }
  
  match producer_kind {
    Producer => assert_eq(true, true)
    _ => @test.fail("Expected Producer")
  }
  
  match consumer_kind {
    Consumer => assert_eq(true, true)
    _ => @test.fail("Expected Consumer")
  }
  
  // 测试所有StatusCode值
  let unset_status = Unset
  let ok_status = Ok
  let error_status = Error
  
  // 验证StatusCode的可用性
  match unset_status {
    Unset => assert_eq(true, true)
    _ => @test.fail("Expected Unset")
  }
  
  match ok_status {
    Ok => assert_eq(true, true)
    _ => @test.fail("Expected Ok")
  }
  
  match error_status {
    Error => assert_eq(true, true)
    _ => @test.fail("Expected Error")
  }
  
  // 测试SpanKind和StatusCode的组合使用
  let test_span = Span::{
    name: "compatibility-test-span",
    context: SpanContext::{
      trace_id: [0x01_byte, 0x02_byte, 0x03_byte, 0x04_byte,
                 0x05_byte, 0x06_byte, 0x07_byte, 0x08_byte,
                 0x09_byte, 0x0A_byte, 0x0B_byte, 0x0C_byte,
                 0x0D_byte, 0x0E_byte, 0x0F_byte, 0x10_byte],
      span_id: [0x11_byte, 0x12_byte, 0x13_byte, 0x14_byte,
                0x15_byte, 0x16_byte, 0x17_byte, 0x18_byte],
      trace_flags: 0x01_byte,
      trace_state: ""
    },
    kind: server_kind,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200001000000L),
    status: ok_status,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  // 验证Span的Kind和Status
  match test_span.kind {
    Server => assert_eq(true, true)
    _ => @test.fail("Expected Server kind")
  }
  
  match test_span.status {
    Ok => assert_eq(true, true)
    _ => @test.fail("Expected Ok status")
  }
}

test "severity_number_compatibility" {
  // 测试SeverityNumber的兼容性
  
  // 测试所有SeverityNumber值
  let trace_severity = Trace
  let debug_severity = Debug
  let info_severity = Info
  let warn_severity = Warn
  let error_severity = Error
  let fatal_severity = Fatal
  
  // 验证SeverityNumber的可用性
  match trace_severity {
    Trace => assert_eq(true, true)
    _ => @test.fail("Expected Trace")
  }
  
  match debug_severity {
    Debug => assert_eq(true, true)
    _ => @test.fail("Expected Debug")
  }
  
  match info_severity {
    Info => assert_eq(true, true)
    _ => @test.fail("Expected Info")
  }
  
  match warn_severity {
    Warn => assert_eq(true, true)
    _ => @test.fail("Expected Warn")
  }
  
  match error_severity {
    Error => assert_eq(true, true)
    _ => @test.fail("Expected Error")
  }
  
  match fatal_severity {
    Fatal => assert_eq(true, true)
    _ => @test.fail("Expected Fatal")
  }
  
  // 测试SeverityNumber在LogRecord中的使用
  let test_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: info_severity,
    severity_text: Some("INFO"),
    body: Some("Compatibility test log"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证LogRecord的SeverityNumber
  match test_log.severity_number {
    Info => assert_eq(true, true)
    _ => @test.fail("Expected Info severity")
  }
}

test "instrument_type_compatibility" {
  // 测试InstrumentType的兼容性
  
  // 测试所有InstrumentType值
  let counter_type = Counter
  let histogram_type = Histogram
  let up_down_counter_type = UpDownCounter
  let gauge_type = Gauge
  let observable_counter_type = ObservableCounter
  let observable_gauge_type = ObservableGauge
  let observable_up_down_counter_type = ObservableUpDownCounter
  
  // 验证InstrumentType的可用性
  match counter_type {
    Counter => assert_eq(true, true)
    _ => @test.fail("Expected Counter")
  }
  
  match histogram_type {
    Histogram => assert_eq(true, true)
    _ => @test.fail("Expected Histogram")
  }
  
  match up_down_counter_type {
    UpDownCounter => assert_eq(true, true)
    _ => @test.fail("Expected UpDownCounter")
  }
  
  match gauge_type {
    Gauge => assert_eq(true, true)
    _ => @test.fail("Expected Gauge")
  }
  
  match observable_counter_type {
    ObservableCounter => assert_eq(true, true)
    _ => @test.fail("Expected ObservableCounter")
  }
  
  match observable_gauge_type {
    ObservableGauge => assert_eq(true, true)
    _ => @test.fail("Expected ObservableGauge")
  }
  
  match observable_up_down_counter_type {
    ObservableUpDownCounter => assert_eq(true, true)
    _ => @test.fail("Expected ObservableUpDownCounter")
  }
}

test "noop_implementations_compatibility" {
  // 测试No-op实现的兼容性
  
  // 测试No-op Tracer
  let noop_tracer = NoopTracer::{}
  let test_context = Context::empty()
  
  let (result_context, result_span) = noop_tracer.start_span(
    test_context,
    "noop-test-span",
    Some(Server),
    Some([("test.attr", AttributeValue::string("test-value"))]),
    Some(1640995200000000000L)
  )
  
  // 验证No-op Tracer的结果
  assert_eq(result_span.name, "noop-test-span")
  match result_span.kind {
    Server => assert_eq(true, true)
    _ => @test.fail("Expected Server kind")
  }
  assert_eq(result_span.attributes.length(), 1)
  match result_span.attributes[0] {
    ("test.attr", AttributeValue::string(value)) => assert_eq(value, "test-value")
    _ => @test.fail("Expected test attribute")
  }
  
  // 测试No-op TracerProvider
  let noop_tracer_provider = NoopTracerProvider::{}
  let retrieved_tracer = noop_tracer_provider.get_tracer("test-tracer", Some("1.0.0"))
  
  // 验证No-op TracerProvider返回No-op Tracer
  // 注意：这里我们无法直接验证类型，但可以验证它具有相同的接口
  
  // 测试No-op Meter
  let noop_meter = NoopMeter::{}
  let noop_counter = noop_meter.create_counter("test-counter", Some("count"), Some("Test counter"))
  let noop_histogram = noop_meter.create_histogram("test-histogram", Some("ms"), Some("Test histogram"))
  let noop_up_down_counter = noop_meter.create_up_down_counter("test-up-down-counter", None, None)
  let noop_gauge = noop_meter.create_gauge("test-gauge", Some("value"), Some("Test gauge"))
  
  // 测试No-op操作不会抛出错误
  noop_counter.add(100L, Some([("test.key", AttributeValue::string("test.value"))]))
  noop_histogram.record(50.0, Some([("test.key", AttributeValue::string("test.value"))]))
  noop_up_down_counter.add(-25L, Some([("test.key", AttributeValue::string("test.value"))]))
  noop_gauge.record(75.0, Some([("test.key", AttributeValue::string("test.value"))]))
  
  // 测试No-op MeterProvider
  let noop_meter_provider = NoopMeterProvider::{}
  let retrieved_meter = noop_meter_provider.get_meter("test-meter", Some("1.0.0"), Some("http://example.com/schema"))
  
  // 测试No-op Logger
  let noop_logger = NoopLogger::{}
  let test_log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("No-op test log"),
    attributes: [("test.key", AttributeValue::string("test.value"))],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 测试No-op Logger操作不会抛出错误
  noop_logger.emit(test_log_record)
  noop_logger.debug("Debug message", Some([("test.key", AttributeValue::string("test.value"))]))
  noop_logger.info("Info message", Some([("test.key", AttributeValue::string("test.value"))]))
  noop_logger.warn("Warning message", Some([("test.key", AttributeValue::string("test.value"))]))
  noop_logger.error("Error message", Some([("test.key", AttributeValue::string("test.value"))]))
  noop_logger.fatal("Fatal message", Some([("test.key", AttributeValue::string("test.value"))]))
  
  // 测试No-op LoggerProvider
  let noop_logger_provider = NoopLoggerProvider::{}
  let retrieved_logger = noop_logger_provider.get_logger("test-logger", Some("1.0.0"), Some("http://example.com/schema"))
}

test "resource_default_compatibility" {
  // 测试Resource默认构造函数的兼容性
  
  // 测试Resource::default函数
  let default_resource = Resource::default("test-service")
  
  // 验证默认Resource的字段
  assert_eq(default_resource.service_name, "test-service")
  match default_resource.service_version {
    None => assert_eq(true, true) // 预期为None
    Some(_) => @test.fail("Expected service_version to be None in default")
  }
  assert_eq(default_resource.telemetry_sdk_name, "azimuth")
  assert_eq(default_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(default_resource.attributes.length(), 0)
  
  // 测试具有不同服务名的Resource
  let payment_resource = Resource::default("payment-service")
  let order_resource = Resource::default("order-service")
  let user_resource = Resource::default("user-service")
  
  assert_eq(payment_resource.service_name, "payment-service")
  assert_eq(order_resource.service_name, "order-service")
  assert_eq(user_resource.service_name, "user-service")
  
  // 验证所有默认Resource的其他字段相同
  assert_eq(payment_resource.telemetry_sdk_name, order_resource.telemetry_sdk_name)
  assert_eq(order_resource.telemetry_sdk_name, user_resource.telemetry_sdk_name)
  assert_eq(user_resource.telemetry_sdk_name, "azimuth")
  
  assert_eq(payment_resource.telemetry_sdk_version, order_resource.telemetry_sdk_version)
  assert_eq(order_resource.telemetry_sdk_version, user_resource.telemetry_sdk_version)
  assert_eq(user_resource.telemetry_sdk_version, "0.1.0")
}

test "propagation_header_compatibility" {
  // 测试传播头部的兼容性
  
  // 验证W3C标准头部常量
  assert_eq(TRACE_PARENT_HEADER, "traceparent")
  assert_eq(TRACE_STATE_HEADER, "tracestate")
  assert_eq(BAGGAGE_HEADER, "baggage")
  
  // 测试MapCarrier的兼容性
  let carrier_data = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"),
    ("baggage", "key1=value1,key2=value2"),
    ("custom-header", "custom-value")
  ]
  
  let carrier = MapCarrier::from_map(carrier_data)
  
  // 验证MapCarrier的基本操作
  match carrier.get("traceparent") {
    Some(trace_parent) => {
      assert_eq(trace_parent.contains("0af7651916cd43dd8448eb211c80319c"), true)
      assert_eq(trace_parent.contains("b7ad6b7169203331"), true)
      assert_eq(trace_parent.contains("01"), true)
    }
    None => @test.fail("Expected traceparent header")
  }
  
  match carrier.get("tracestate") {
    Some(trace_state) => {
      assert_eq(trace_state.contains("rojo=00f067aa0ba902b7"), true)
      assert_eq(trace_state.contains("congo=t61rcWkgMzE"), true)
    }
    None => @test.fail("Expected tracestate header")
  }
  
  match carrier.get("baggage") {
    Some(baggage) => {
      assert_eq(baggage.contains("key1=value1"), true)
      assert_eq(baggage.contains("key2=value2"), true)
    }
    None => @test.fail("Expected baggage header")
  }
  
  match carrier.get("custom-header") {
    Some(custom_value) => assert_eq(custom_value, "custom-value")
    None => @test.fail("Expected custom header")
  }
  
  match carrier.get("non-existent-header") {
    Some(_) => @test.fail("Should not find non-existent header")
    None => assert_eq(true, true) // 预期的结果
  }
  
  // 验证keys方法
  let keys = carrier.keys()
  assert_eq(keys.length(), 4)
  assert_eq(keys.contains("traceparent"), true)
  assert_eq(keys.contains("tracestate"), true)
  assert_eq(keys.contains("baggage"), true)
  assert_eq(keys.contains("custom-header"), true)
}