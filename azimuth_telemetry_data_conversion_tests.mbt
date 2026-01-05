// Azimuth Telemetry Data Conversion Test Suite
// 测试遥测数据在不同格式之间的转换功能

test "属性值类型转换测试" {
  // 测试字符串到整数的转换
  let string_attr = StringValue("42")
  let converted_int = AttributeValueConverter::to_int(string_attr)
  match converted_int {
    Some(IntValue(42)) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试字符串到浮点数的转换
  let string_float_attr = StringValue("3.14")
  let converted_float = AttributeValueConverter::to_float(string_float_attr)
  match converted_float {
    Some(FloatValue(3.14)) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试字符串到布尔值的转换
  let string_bool_attr = StringValue("true")
  let converted_bool = AttributeValueConverter::to_bool(string_bool_attr)
  match converted_bool {
    Some(BoolValue(true)) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试整数到字符串的转换
  let int_attr = IntValue(100)
  let converted_string = AttributeValueConverter::to_string(int_attr)
  match converted_string {
    Some(StringValue("100")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试浮点数到字符串的转换
  let float_attr = FloatValue(2.718)
  let converted_float_string = AttributeValueConverter::to_string(float_attr)
  match converted_float_string {
    Some(StringValue("2.718")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试布尔值到字符串的转换
  let bool_attr = BoolValue(false)
  let converted_bool_string = AttributeValueConverter::to_string(bool_attr)
  match converted_bool_string {
    Some(StringValue("false")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试无效转换
  let invalid_string = StringValue("not_a_number")
  let invalid_conversion = AttributeValueConverter::to_int(invalid_string)
  match invalid_conversion {
    None => assert_true(true)
    _ => assert_true(false)
  }
}

test "属性集合格式转换测试" {
  // 创建属性集合
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  
  // 测试转换为JSON格式
  let json_representation = AttributesConverter::to_json(attrs)
  assert_true(json_representation.contains("\"string.key\":\"test_value\""))
  assert_true(json_representation.contains("\"int.key\":42"))
  assert_true(json_representation.contains("\"float.key\":3.14"))
  assert_true(json_representation.contains("\"bool.key\":true"))
  
  // 测试从JSON格式转换
  let parsed_attrs = AttributesConverter::from_json(json_representation)
  let string_result = Attributes::get(parsed_attrs, "string.key")
  match string_result {
    Some(StringValue(v)) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  let int_result = Attributes::get(parsed_attrs, "int.key")
  match int_result {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // 测试转换为XML格式
  let xml_representation = AttributesConverter::to_xml(attrs)
  assert_true(xml_representation.contains("<attribute name=\"string.key\" type=\"string\">test_value</attribute>"))
  assert_true(xml_representation.contains("<attribute name=\"int.key\" type=\"int\">42</attribute>"))
  assert_true(xml_representation.contains("<attribute name=\"float.key\" type=\"float\">3.14</attribute>"))
  assert_true(xml_representation.contains("<attribute name=\"bool.key\" type=\"bool\">true</attribute>"))
  
  // 测试从XML格式转换
  let parsed_xml_attrs = AttributesConverter::from_xml(xml_representation)
  let float_result = Attributes::get(parsed_xml_attrs, "float.key")
  match float_result {
    Some(FloatValue(v)) => assert_eq(v, 3.14)
    _ => assert_true(false)
  }
  
  let bool_result = Attributes::get(parsed_xml_attrs, "bool.key")
  match bool_result {
    Some(BoolValue(v)) => assert_true(v)
    _ => assert_true(false)
  }
}

test "跨度上下文格式转换测试" {
  // 创建跨度上下文
  let span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "rojo=00f067aa0ba902b7")
  
  // 测试转换为W3C traceparent格式
  let traceparent_format = SpanContextConverter::to_traceparent(span_ctx)
  assert_eq(traceparent_format, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // 测试从W3C traceparent格式转换
  let parsed_span_ctx = SpanContextConverter::from_traceparent(traceparent_format)
  assert_eq(SpanContext::trace_id(parsed_span_ctx), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(SpanContext::span_id(parsed_span_ctx), "b7ad6b7169203331")
  assert_true(SpanContext::is_sampled(parsed_span_ctx))
  
  // 测试转换为二进制格式
  let binary_format = SpanContextConverter::to_binary(span_ctx)
  assert_eq(binary_format.length(), 29) // 16字节trace_id + 8字节span_id + 1字节flags + 4字节trace_state长度
  
  // 测试从二进制格式转换
  let parsed_binary_ctx = SpanContextConverter::from_binary(binary_format)
  assert_eq(SpanContext::trace_id(parsed_binary_ctx), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(SpanContext::span_id(parsed_binary_ctx), "b7ad6b7169203331")
  assert_true(SpanContext::is_sampled(parsed_binary_ctx))
  
  // 测试无效traceparent格式处理
  let invalid_traceparent = "invalid-format"
  let invalid_parsed_ctx = SpanContextConverter::from_traceparent(invalid_traceparent)
  assert_false(SpanContext::is_valid(invalid_parsed_ctx))
}

test "日志记录格式转换测试" {
  // 创建日志记录
  let log_attrs = Attributes::new()
  Attributes::set(log_attrs, "user.id", StringValue("user123"))
  Attributes::set(log_attrs, "operation", StringValue("login"))
  
  let log_record = LogRecord::new_with_context(
    Error,
    Some("User login failed"),
    Some(log_attrs),
    Some(1609459200000L),
    Some(1609459200100L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    Some(Context::root())
  )
  
  // 测试转换为JSON格式
  let json_log = LogRecordConverter::to_json(log_record)
  assert_true(json_log.contains("\"severityNumber\":\"ERROR\""))
  assert_true(json_log.contains("\"body\":\"User login failed\""))
  assert_true(json_log.contains("\"user.id\":\"user123\""))
  assert_true(json_log.contains("\"operation\":\"login\""))
  assert_true(json_log.contains("\"traceId\":\"0af7651916cd43dd8448eb211c80319c\""))
  assert_true(json_log.contains("\"spanId\":\"b7ad6b7169203331\""))
  
  // 测试从JSON格式转换
  let parsed_log = LogRecordConverter::from_json(json_log)
  assert_eq(LogRecord::severity_number(parsed_log), Error)
  match LogRecord::body(parsed_log) {
    Some(body) => assert_eq(body, "User login failed")
    None => assert_true(false)
  }
  
  // 测试转换为纯文本格式
  let text_log = LogRecordConverter::to_text(log_record)
  assert_true(text_log.contains("[ERROR] User login failed"))
  assert_true(text_log.contains("user.id=user123"))
  assert_true(text_log.contains("operation=login"))
  assert_true(text_log.contains("trace_id=0af7651916cd43dd8448eb211c80319c"))
  assert_true(text_log.contains("span_id=b7ad6b7169203331"))
  
  // 测试转换为结构化格式
  let structured_log = LogRecordConverter::to_structured(log_record)
  assert_true(structured_log.contains("timestamp=1609459200000"))
  assert_true(structured_log.contains("level=ERROR"))
  assert_true(structured_log.contains("msg=\"User login failed\""))
  assert_true(structured_log.contains("user_id=user123"))
  assert_true(structured_log.contains("operation=login"))
}

test "指标数据格式转换测试" {
  // 创建指标数据
  let metric_attrs = Attributes::new()
  Attributes::set(metric_attrs, "service.name", StringValue("auth-service"))
  Attributes::set(metric_attrs, "endpoint", StringValue("/api/login"))
  
  // 测试计数器指标转换
  let counter_metric = MetricData::Counter {
    name: "login_attempts",
    description: "Number of login attempts",
    unit: "count",
    value: 100.0,
    attributes: metric_attrs
  }
  
  let counter_json = MetricConverter::to_json(counter_metric)
  assert_true(counter_json.contains("\"type\":\"counter\""))
  assert_true(counter_json.contains("\"name\":\"login_attempts\""))
  assert_true(counter_json.contains("\"value\":100.0"))
  assert_true(counter_json.contains("\"service.name\":\"auth-service\""))
  
  let parsed_counter = MetricConverter::from_json(counter_json)
  match parsed_counter {
    MetricData::Counter { name, value, ... } => {
      assert_eq(name, "login_attempts")
      assert_eq(value, 100.0)
    }
    _ => assert_true(false)
  }
  
  // 测试直方图指标转换
  let histogram_metric = MetricData::Histogram {
    name: "request_duration",
    description: "Request duration histogram",
    unit: "ms",
    buckets: [(0.0, 10.0), (10.0, 50.0), (50.0, 100.0), (100.0, 1000.0)],
    counts: [5, 25, 15, 5],
    sum: 1250.0,
    count: 50,
    attributes: metric_attrs
  }
  
  let histogram_json = MetricConverter::to_json(histogram_metric)
  assert_true(histogram_json.contains("\"type\":\"histogram\""))
  assert_true(histogram_json.contains("\"name\":\"request_duration\""))
  assert_true(histogram_json.contains("\"sum\":1250.0"))
  assert_true(histogram_json.contains("\"count\":50"))
  
  let parsed_histogram = MetricConverter::from_json(histogram_json)
  match parsed_histogram {
    MetricData::Histogram { name, sum, count, ... } => {
      assert_eq(name, "request_duration")
      assert_eq(sum, 1250.0)
      assert_eq(count, 50)
    }
    _ => assert_true(false)
  }
  
  // 测试仪表指标转换
  let gauge_metric = MetricData::Gauge {
    name: "memory_usage",
    description: "Current memory usage",
    unit: "bytes",
    value: 1073741824.0,
    attributes: metric_attrs
  }
  
  let gauge_json = MetricConverter::to_json(gauge_metric)
  assert_true(gauge_json.contains("\"type\":\"gauge\""))
  assert_true(gauge_json.contains("\"name\":\"memory_usage\""))
  assert_true(gauge_json.contains("\"value\":1073741824.0"))
  
  let parsed_gauge = MetricConverter::from_json(gauge_json)
  match parsed_gauge {
    MetricData::Gauge { name, value, ... } => {
      assert_eq(name, "memory_usage")
      assert_eq(value, 1073741824.0)
    }
    _ => assert_true(false)
  }
}

test "资源数据格式转换测试" {
  // 创建资源
  let resource_attrs = [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("instance-456")),
    ("host.name", StringValue("prod-server-01")),
    ("host.ip", StringValue("192.168.1.100")),
    ("process.pid", IntValue(12345)),
    ("process.runtime.name", StringValue("node")),
    ("process.runtime.version", StringValue("16.14.0")),
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.version", StringValue("1.0.0"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), resource_attrs)
  
  // 测试转换为JSON格式
  let resource_json = ResourceConverter::to_json(resource)
  assert_true(resource_json.contains("\"service.name\":\"payment-service\""))
  assert_true(resource_json.contains("\"service.version\":\"2.1.0\""))
  assert_true(resource_json.contains("\"process.pid\":12345"))
  assert_true(resource_json.contains("\"telemetry.sdk.name\":\"azimuth\""))
  
  // 测试从JSON格式转换
  let parsed_resource = ResourceConverter::from_json(resource_json)
  let service_name = Resource::get_attribute(parsed_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "payment-service")
    _ => assert_true(false)
  }
  
  let process_pid = Resource::get_attribute(parsed_resource, "process.pid")
  match process_pid {
    Some(IntValue(pid)) => assert_eq(pid, 12345)
    _ => assert_true(false)
  }
  
  // 测试转换为环境变量格式
  let env_format = ResourceConverter::to_env(resource)
  assert_true(env_format.contains("SERVICE_NAME=payment-service"))
  assert_true(env_format.contains("SERVICE_VERSION=2.1.0"))
  assert_true(env_format.contains("PROCESS_PID=12345"))
  assert_true(env_format.contains("TELEMETRY_SDK_NAME=azimuth"))
  
  // 测试转换为Prometheus格式
  let prometheus_format = ResourceConverter::to_prometheus(resource)
  assert_true(prometheus_format.contains("service_name=\"payment-service\""))
  assert_true(prometheus_format.contains("service_version=\"2.1.0\""))
  assert_true(prometheus_format.contains("process_pid=\"12345\""))
  assert_true(prometheus_format.contains("telemetry_sdk_name=\"azimuth\""))
}

test "行李数据格式转换测试" {
  // 创建行李数据
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "user123")
  let updated_baggage2 = Baggage::set_entry(updated_baggage, "session.id", "session456")
  let updated_baggage3 = Baggage::set_entry(updated_baggage2, "request.id", "req789")
  let final_baggage = Baggage::set_entry(updated_baggage3, "tenant.id", "tenant001")
  
  // 测试转换为W3C baggage格式
  let w3c_format = BaggageConverter::to_w3c(final_baggage)
  assert_true(w3c_format.contains("user.id=user123"))
  assert_true(w3c_format.contains("session.id=session456"))
  assert_true(w3c_format.contains("request.id=req789"))
  assert_true(w3c_format.contains("tenant.id=tenant001"))
  
  // 测试从W3C baggage格式转换
  let parsed_baggage = BaggageConverter::from_w3c(w3c_format)
  let user_id = Baggage::get_entry(parsed_baggage, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  let session_id = Baggage::get_entry(parsed_baggage, "session.id")
  match session_id {
    Some(value) => assert_eq(value, "session456")
    None => assert_true(false)
  }
  
  // 测试转换为HTTP头格式
  let http_header_format = BaggageConverter::to_http_header(final_baggage)
  assert_true(http_header_format.contains("baggage: "))
  assert_true(http_header_format.contains("user.id=user123"))
  assert_true(http_header_format.contains("session.id=session456"))
  
  // 测试从HTTP头格式转换
  let parsed_http_baggage = BaggageConverter::from_http_header(http_header_format)
  let request_id = Baggage::get_entry(parsed_http_baggage, "request.id")
  match request_id {
    Some(value) => assert_eq(value, "req789")
    None => assert_true(false)
  }
  
  // 测试转换为JSON格式
  let baggage_json = BaggageConverter::to_json(final_baggage)
  assert_true(baggage_json.contains("\"user.id\":\"user123\""))
  assert_true(baggage_json.contains("\"session.id\":\"session456\""))
  assert_true(baggage_json.contains("\"request.id\":\"req789\""))
  assert_true(baggage_json.contains("\"tenant.id\":\"tenant001\""))
  
  // 测试从JSON格式转换
  let parsed_json_baggage = BaggageConverter::from_json(baggage_json)
  let tenant_id = Baggage::get_entry(parsed_json_baggage, "tenant.id")
  match tenant_id {
    Some(value) => assert_eq(value, "tenant001")
    None => assert_true(false)
  }
}

test "载体数据格式转换测试" {
  // 创建文本映射载体
  let carrier_headers = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"),
    ("x-correlation-id", "corr-123456"),
    ("x-request-id", "req-789012"),
    ("x-user-id", "user-345678"),
    ("x-tenant-id", "tenant-901234")
  ]
  
  let text_map_carrier = TextMapCarrier::new(carrier_headers)
  
  // 测试转换为HTTP头格式
  let http_headers = CarrierConverter::to_http_headers(text_map_carrier)
  assert_true(http_headers.contains("traceparent: 00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_true(http_headers.contains("tracestate: rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"))
  assert_true(http_headers.contains("x-correlation-id: corr-123456"))
  assert_true(http_headers.contains("x-request-id: req-789012"))
  
  // 测试从HTTP头格式转换
  let parsed_carrier = CarrierConverter::from_http_headers(http_headers)
  let traceparent = TextMapCarrier::get(parsed_carrier, "traceparent")
  match traceparent {
    Some(value) => assert_eq(value, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    None => assert_true(false)
  }
  
  let correlation_id = TextMapCarrier::get(parsed_carrier, "x-correlation-id")
  match correlation_id {
    Some(value) => assert_eq(value, "corr-123456")
    None => assert_true(false)
  }
  
  // 测试转换为JSON格式
  let carrier_json = CarrierConverter::to_json(text_map_carrier)
  assert_true(carrier_json.contains("\"traceparent\":\"00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01\""))
  assert_true(carrier_json.contains("\"tracestate\":\"rojo=00f067aa0ba902b7,congo=t61rcWkgMzE\""))
  assert_true(carrier_json.contains("\"x-correlation-id\":\"corr-123456\""))
  
  // 测试从JSON格式转换
  let parsed_json_carrier = CarrierConverter::from_json(carrier_json)
  let request_id = TextMapCarrier::get(parsed_json_carrier, "x-request-id")
  match request_id {
    Some(value) => assert_eq(value, "req-789012")
    None => assert_true(false)
  }
  
  // 测试转换为环境变量格式
  let env_format = CarrierConverter::to_env(text_map_carrier)
  assert_true(env_format.contains("TRACERPARENT=00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_true(env_format.contains("TRACESTATE=rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"))
  assert_true(env_format.contains("X_CORRELATION_ID=corr-123456"))
  
  // 测试从环境变量格式转换
  let parsed_env_carrier = CarrierConverter::from_env(env_format)
  let user_id = TextMapCarrier::get(parsed_env_carrier, "x-user-id")
  match user_id {
    Some(value) => assert_eq(value, "user-345678")
    None => assert_true(false)
  }
}

test "复合数据结构转换测试" {
  // 创建复合数据结构
  let span_ctx = SpanContext::new("trace123", "span456", true, "state789")
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0"))
  ])
  
  let baggage = Baggage::new()
  let baggage_with_entries = Baggage::set_entry(baggage, "user.id", "user123")
  
  let span = Span::new("test-span", Server, span_ctx)
  Span::set_attribute(span, "operation.type", StringValue("http"))
  Span::set_attribute(span, "http.method", StringValue("GET"))
  Span::set_attribute(span, "http.status_code", IntValue(200))
  
  // 测试转换为完整的遥测数据包
  let telemetry_packet = TelemetryPacket::new(span, resource, baggage_with_entries)
  let packet_json = TelemetryPacketConverter::to_json(telemetry_packet)
  
  assert_true(packet_json.contains("\"spanName\":\"test-span\""))
  assert_true(packet_json.contains("\"spanKind\":\"SERVER\""))
  assert_true(packet_json.contains("\"traceId\":\"trace123\""))
  assert_true(packet_json.contains("\"spanId\":\"span456\""))
  assert_true(packet_json.contains("\"service.name\":\"test-service\""))
  assert_true(packet_json.contains("\"operation.type\":\"http\""))
  assert_true(packet_json.contains("\"user.id\":\"user123\""))
  
  // 测试从JSON格式转换
  let parsed_packet = TelemetryPacketConverter::from_json(packet_json)
  let parsed_span = TelemetryPacket::span(parsed_packet)
  assert_eq(Span::name(parsed_span), "test-span")
  match Span::kind(parsed_span) {
    Server => assert_true(true)
    _ => assert_true(false)
  }
  
  let parsed_resource = TelemetryPacket::resource(parsed_packet)
  let service_name = Resource::get_attribute(parsed_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_true(false)
  }
  
  let parsed_baggage = TelemetryPacket::baggage(parsed_packet)
  let user_id = Baggage::get_entry(parsed_baggage, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  // 测试转换为压缩格式
  let compressed_packet = TelemetryPacketConverter::to_compressed(telemetry_packet)
  assert_true(compressed_packet.length() < packet_json.length()) // 压缩后应该更小
  
  // 测试从压缩格式转换
  let decompressed_packet = TelemetryPacketConverter::from_compressed(compressed_packet)
  let decompressed_span = TelemetryPacket::span(decompressed_packet)
  assert_eq(Span::name(decompressed_span), "test-span")
  
  // 测试转换为二进制格式
  let binary_packet = TelemetryPacketConverter::to_binary(telemetry_packet)
  assert_true(binary_packet.length() > 0) // 二进制数据应该非空
  
  // 测试从二进制格式转换
  let parsed_binary_packet = TelemetryPacketConverter::from_binary(binary_packet)
  let binary_span = TelemetryPacket::span(parsed_binary_packet)
  assert_eq(Span::name(binary_span), "test-span")
}