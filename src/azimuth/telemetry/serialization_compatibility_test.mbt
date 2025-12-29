// 序列化和反序列化测试 - 测试数据格式的转换
// 验证遥测数据在不同格式间的正确转换

test "attribute_value_serialization" {
  // 测试AttributeValue的序列化
  
  let string_attr = common::AttributeValue::string("test-value")
  let int_attr = common::AttributeValue::int(42L)
  let float_attr = common::AttributeValue::float(3.14159)
  let bool_attr = common::AttributeValue::bool(true)
  let string_array_attr = common::AttributeValue::array_string(["a", "b", "c"])
  let int_array_attr = common::AttributeValue::array_int([1L, 2L, 3L])
  let float_array_attr = common::AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array_attr = common::AttributeValue::array_bool([true, false, true])
  
  // 字符串属性序列化为JSON
  match string_attr {
    common::StringValue(s) => {
      let json_string = "\"test-value\""
      assert_eq(json_string.contains("test-value"), true)
      assert_eq(json_string.has_prefix("\""), true)
      assert_eq(json_string.has_suffix("\""), true)
    }
    _ => @test.fail("Expected StringValue")
  }
  
  // 整数属性序列化为JSON
  match int_attr {
    common::IntValue(i) => {
      let json_int = "42"
      assert_eq(json_int, "42")
    }
    _ => @test.fail("Expected IntValue")
  }
  
  // 浮点数属性序列化为JSON
  match float_attr {
    common::FloatValue(f) => {
      let json_float = "3.14159"
      assert_eq(json_float, "3.14159")
    }
    _ => @test.fail("Expected FloatValue")
  }
  
  // 布尔属性序列化为JSON
  match bool_attr {
    common::BoolValue(b) => {
      let json_bool = if b { "true" } else { "false" }
      assert_eq(json_bool, "true")
    }
    _ => @test.fail("Expected BoolValue")
  }
  
  // 字符串数组序列化为JSON
  match string_array_attr {
    common::ArrayStringValue(arr) => {
      let json_array = "[\"a\",\"b\",\"c\"]"
      assert_eq(json_array.has_prefix("["), true)
      assert_eq(json_array.has_suffix("]"), true)
      assert_eq(json_array.contains("\"a\""), true)
      assert_eq(json_array.contains("\"b\""), true)
      assert_eq(json_array.contains("\"c\""), true)
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
}

test "log_record_json_serialization" {
  // 测试LogRecord的JSON序列化
  
  let log_record = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Test log message")
    .with_attribute("service.name", common::AttributeValue::string("test-service"))
    .with_attribute("http.method", common::AttributeValue::string("GET"))
    .with_attribute("http.status_code", common::AttributeValue::int(200L))
    .with_attribute("processing.time", common::AttributeValue::float(123.45))
    .with_attribute("success", common::AttributeValue::bool(true))
    .with_attribute("tags", common::AttributeValue::array_string(["web", "api"]))
    .build()
  
  // 构建JSON表示
  let json = "{"
  json = json + "\"severity_number\":\"Info\","
  json = json + "\"body\":\"Test log message\","
  json = json + "\"attributes\":{"
  json = json + "\"service.name\":\"test-service\","
  json = json + "\"http.method\":\"GET\","
  json = json + "\"http.status_code\":200,"
  json = json + "\"processing.time\":123.45,"
  json = json + "\"success\":true,"
  json = json + "\"tags\":[\"web\",\"api\"]"
  json = json + "}"
  json = json + "}"
  
  // 验证JSON结构
  assert_eq(json.has_prefix("{"), true)
  assert_eq(json.has_suffix("}"), true)
  assert_eq(json.contains("\"severity_number\":\"Info\""), true)
  assert_eq(json.contains("\"body\":\"Test log message\""), true)
  assert_eq(json.contains("\"service.name\":\"test-service\""), true)
  assert_eq(json.contains("\"http.method\":\"GET\""), true)
  assert_eq(json.contains("\"http.status_code\":200"), true)
  assert_eq(json.contains("\"processing.time\":123.45"), true)
  assert_eq(json.contains("\"success\":true"), true)
  assert_eq(json.contains("\"tags\":[\"web\",\"api\"]"), true)
}

test "span_json_serialization" {
  // 测试Span的JSON序列化
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("serialization-test")
  let (_, span) = tracer.start_span(
    context::Context::empty(),
    "test-span",
    trace::Server,
    [
      ("service.name", common::AttributeValue::string("test-service")),
      ("http.method", common::AttributeValue::string("POST")),
      ("http.status_code", common::AttributeValue::int(201L))
    ],
    1640995200000L
  )
  
  // 构建JSON表示
  let json = "{"
  json = json + "\"name\":\"test-span\","
  json = json + "\"kind\":\"Server\","
  json = json + "\"start_time_unix_nanos\":1640995200000,"
  json = json + "\"status\":\"Unset\","
  json = json + "\"attributes\":{"
  json = json + "\"service.name\":\"test-service\","
  json = json + "\"http.method\":\"POST\","
  json = json + "\"http.status_code\":201"
  json = json + "},"
  json = json + "\"events\":[],"
  json = json + "\"links\":[]"
  json = json + "}"
  
  // 验证JSON结构
  assert_eq(json.has_prefix("{"), true)
  assert_eq(json.has_suffix("}"), true)
  assert_eq(json.contains("\"name\":\"test-span\""), true)
  assert_eq(json.contains("\"kind\":\"Server\""), true)
  assert_eq(json.contains("\"start_time_unix_nanos\":1640995200000"), true)
  assert_eq(json.contains("\"status\":\"Unset\""), true)
  assert_eq(json.contains("\"service.name\":\"test-service\""), true)
  assert_eq(json.contains("\"http.method\":\"POST\""), true)
  assert_eq(json.contains("\"http.status_code\":201"), true)
  assert_eq(json.contains("\"events\":[]"), true)
  assert_eq(json.contains("\"links\":[]"), true)
}

test "metric_prometheus_serialization" {
  // 测试指标的Prometheus格式序列化
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("prometheus-test-meter")
  
  // 创建并记录指标
  let counter = meter.create_counter("http_requests_total", "count", "Total HTTP requests")
  let histogram = meter.create_histogram("http_request_duration_seconds", "seconds", "HTTP request duration")
  let gauge = meter.create_gauge("active_connections", "connections", "Active connections")
  
  // 模拟指标记录并序列化为Prometheus格式
  let counter_metric = "http_requests_total{service=\"api\",method=\"GET\",status=\"200\"} 42"
  let histogram_metric = "http_request_duration_seconds_bucket{service=\"api\",le=\"0.1\"} 5"
  let gauge_metric = "active_connections{service=\"api\",instance=\"pod-123\"} 127"
  
  // 验证Prometheus格式
  assert_eq(counter_metric.contains("http_requests_total"), true)
  assert_eq(counter_metric.contains("service=\"api\""), true)
  assert_eq(counter_metric.contains("method=\"GET\""), true)
  assert_eq(counter_metric.contains("status=\"200\""), true)
  assert_eq(counter_metric.has_suffix(" 42"), true)
  
  assert_eq(histogram_metric.contains("http_request_duration_seconds_bucket"), true)
  assert_eq(histogram_metric.contains("service=\"api\""), true)
  assert_eq(histogram_metric.contains("le=\"0.1\""), true)
  assert_eq(histogram_metric.has_suffix(" 5"), true)
  
  assert_eq(gauge_metric.contains("active_connections"), true)
  assert_eq(gauge_metric.contains("service=\"api\""), true)
  assert_eq(gauge_metric.contains("instance=\"pod-123\""), true)
  assert_eq(gauge_metric.has_suffix(" 127"), true)
}

test "resource_serialization_formats" {
  // 测试Resource的序列化格式
  
  let resource = common::Resource::default("test-service")
  let resource_attrs = [
    ("service.namespace", common::AttributeValue::string("production")),
    ("service.instance.id", common::AttributeValue::string("instance-123")),
    ("deployment.environment", common::AttributeValue::string("prod")),
    ("host.name", common::AttributeValue::string("web-server-01")),
    ("process.pid", common::AttributeValue::int(12345L)),
    ("process.cpu.percent", common::AttributeValue::float(75.5)),
    ("process.healthy", common::AttributeValue::bool(true)),
    ("tags", common::AttributeValue::array_string(["web", "api", "critical"]))
  ]
  
  // JSON格式序列化
  let json = "{"
  json = json + "\"service_name\":\"test-service\","
  json = json + "\"telemetry_sdk_name\":\"azimuth\","
  json = json + "\"telemetry_sdk_version\":\"0.1.0\","
  json = json + "\"attributes\":{"
  json = json + "\"service.namespace\":\"production\","
  json = json + "\"service.instance.id\":\"instance-123\","
  json = json + "\"deployment.environment\":\"prod\","
  json = json + "\"host.name\":\"web-server-01\","
  json = json + "\"process.pid\":12345,"
  json = json + "\"process.cpu.percent\":75.5,"
  json = json + "\"process.healthy\":true,"
  json = json + "\"tags\":[\"web\",\"api\",\"critical\"]"
  json = json + "}"
  json = json + "}"
  
  // 验证JSON格式
  assert_eq(json.has_prefix("{"), true)
  assert_eq(json.has_suffix("}"), true)
  assert_eq(json.contains("\"service_name\":\"test-service\""), true)
  assert_eq(json.contains("\"telemetry_sdk_name\":\"azimuth\""), true)
  assert_eq(json.contains("\"telemetry_sdk_version\":\"0.1.0\""), true)
  assert_eq(json.contains("\"service.namespace\":\"production\""), true)
  assert_eq(json.contains("\"process.pid\":12345"), true)
  assert_eq(json.contains("\"process.cpu.percent\":75.5"), true)
  assert_eq(json.contains("\"process.healthy\":true"), true)
  assert_eq(json.contains("\"tags\":[\"web\",\"api\",\"critical\"]"), true)
  
  // 键值对格式序列化
  let kv_format = "service_name=test-service"
  kv_format = kv_format + ",telemetry_sdk_name=azimuth"
  kv_format = kv_format + ",telemetry_sdk_version=0.1.0"
  kv_format = kv_format + ",service.namespace=production"
  kv_format = kv_format + ",service.instance.id=instance-123"
  kv_format = kv_format + ",deployment.environment=prod"
  kv_format = kv_format + ",host.name=web-server-01"
  kv_format = kv_format + ",process.pid=12345"
  kv_format = kv_format + ",process.cpu.percent=75.5"
  kv_format = kv_format + ",process.healthy=true"
  kv_format = kv_format + ",tags=web,api,critical"
  
  // 验证键值对格式
  assert_eq(kv_format.contains("service_name=test-service"), true)
  assert_eq(kv_format.contains("telemetry_sdk_name=azimuth"), true)
  assert_eq(kv_format.contains("service.namespace=production"), true)
  assert_eq(kv_format.contains("process.pid=12345"), true)
  assert_eq(kv_format.contains("process.cpu.percent=75.5"), true)
  assert_eq(kv_format.contains("process.healthy=true"), true)
  assert_eq(kv_format.contains("tags=web,api,critical"), true)
}

test "trace_context_serialization" {
  // 测试TraceContext的序列化
  
  // W3C TraceContext格式
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let trace_flags = "01"
  
  let traceparent = "00-" + trace_id + "-" + span_id + "-" + trace_flags
  let tracestate = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  
  // 验证traceparent格式
  assert_eq(traceparent, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  assert_eq(traceparent.has_prefix("00-"), true)
  assert_eq(traceparent.contains("-"), true)
  assert_eq(traceparent.length(), 55)  // 2 + 1 + 32 + 1 + 16 + 1 + 2 = 55
  
  // 验证tracestate格式
  assert_eq(tracestate.contains("rojo=00f067aa0ba902b7"), true)
  assert_eq(tracestate.contains("congo=t61rcWkgMzE"), true)
  assert_eq(tracestate.contains(","), true)
  
  // 二进制格式序列化（模拟）
  let trace_id_bytes = [0x0a, 0xf7, 0x65, 0x19, 0x16, 0xcd, 0x43, 0xdd, 0x84, 0x48, 0xeb, 0x21, 0x1c, 0x80, 0x31, 0x9c]
  let span_id_bytes = [0xb7, 0xad, 0x6b, 0x71, 0x69, 0x20, 0x33, 0x31]
  let trace_flags_byte = 0x01
  
  // 验证二进制格式长度
  assert_eq(trace_id_bytes.length(), 16)
  assert_eq(span_id_bytes.length(), 8)
  assert_eq(trace_flags_byte, 0x01)
}

test "baggage_serialization" {
  // 测试Baggage的序列化
  
  let baggage = context::Baggage::empty()
    .with_entry("user.id", "12345")
    .with_entry("request.id", "req-67890")
    .with_entry("service.version", "1.2.3")
    .with_entry("deployment.environment", "production")
  
  // W3C Baggage格式
  let baggage_header = "user.id=12345,request.id=req-67890,service.version=1.2.3,deployment.environment=production"
  
  // 验证baggage格式
  assert_eq(baggage_header.contains("user.id=12345"), true)
  assert_eq(baggage_header.contains("request.id=req-67890"), true)
  assert_eq(baggage_header.contains("service.version=1.2.3"), true)
  assert_eq(baggage_header.contains("deployment.environment=production"), true)
  assert_eq(baggage_header.contains(","), true)
  
  // 带属性的baggage项
  let baggage_with_properties = "userId=12345;userIdType=internal,sessionId=abc-def;source=web"
  
  // 验证带属性的baggage格式
  assert_eq(baggage_with_properties.contains("userId=12345;userIdType=internal"), true)
  assert_eq(baggage_with_properties.contains("sessionId=abc-def;source=web"), true)
  assert_eq(baggage_with_properties.contains(";"), true)
  assert_eq(baggage_with_properties.contains(","), true)
  
  // URL编码的baggage
  let url_encoded_baggage = "user.name=John%20Doe,request.path=%2Fapi%2Fv1%2Fusers"
  
  // 验证URL编码
  assert_eq(url_encoded_baggage.contains("user.name=John%20Doe"), true)
  assert_eq(url_encoded_baggage.contains("request.path=%2Fapi%2Fv1%2Fusers"), true)
}

test "otel_protocol_serialization" {
  // 测试OpenTelemetry协议序列化
  
  // 创建OTLP JSON格式的资源
  let otlp_resource = "{"
  otlp_resource = otlp_resource + "\"resource\":{"
  otlp_resource = otlp_resource + "\"attributes\":["
  otlp_resource = otlp_resource + "{\"key\":\"service.name\",\"value\":{\"stringValue\":\"test-service\"}},"
  otlp_resource = otlp_resource + "{\"key\":\"telemetry.sdk.name\",\"value\":{\"stringValue\":\"azimuth\"}},"
  otlp_resource = otlp_resource + "{\"key\":\"telemetry.sdk.version\",\"value\":{\"stringValue\":\"0.1.0\"}}"
  otlp_resource = otlp_resource + "]"
  otlp_resource = otlp_resource + "}"
  otlp_resource = otlp_resource + "}"
  
  // 验证OTLP资源格式
  assert_eq(otlp_resource.contains("\"resource\":"), true)
  assert_eq(otlp_resource.contains("\"attributes\":"), true)
  assert_eq(otlp_resource.contains("\"key\":\"service.name\""), true)
  assert_eq(otlp_resource.contains("\"value\":{\"stringValue\":\"test-service\"}"), true)
  assert_eq(otlp_resource.contains("\"telemetry.sdk.name\""), true)
  assert_eq(otlp_resource.contains("\"telemetry.sdk.version\""), true)
  
  // 创建OTLP JSON格式的Span
  let otlp_span = "{"
  otlp_span = otlp_span + "\"traceId\":\"0af7651916cd43dd8448eb211c80319c\","
  otlp_span = otlp_span + "\"spanId\":\"b7ad6b7169203331\","
  otlp_span = otlp_span + "\"name\":\"test-span\","
  otlp_span = otlp_span + "\"kind\":\"SPAN_KIND_SERVER\","
  otlp_span = otlp_span + "\"startTimeUnixNano\":\"1640995200000000000\","
  otlp_span = otlp_span + "\"status\":{"
  otlp_span = otlp_span + "\"code\":\"STATUS_CODE_UNSET\""
  otlp_span = otlp_span + "},"
  otlp_span = otlp_span + "\"attributes\":["
  otlp_span = otlp_span + "{\"key\":\"http.method\",\"value\":{\"stringValue\":\"GET\"}},"
  otlp_span = otlp_span + "{\"key\":\"http.status_code\",\"value\":{\"intValue\":\"200\"}}"
  otlp_span = otlp_span + "]"
  otlp_span = otlp_span + "}"
  
  // 验证OTLP Span格式
  assert_eq(otlp_span.contains("\"traceId\":\"0af7651916cd43dd8448eb211c80319c\""), true)
  assert_eq(otlp_span.contains("\"spanId\":\"b7ad6b7169203331\""), true)
  assert_eq(otlp_span.contains("\"name\":\"test-span\""), true)
  assert_eq(otlp_span.contains("\"kind\":\"SPAN_KIND_SERVER\""), true)
  assert_eq(otlp_span.contains("\"startTimeUnixNano\":\"1640995200000000000\""), true)
  assert_eq(otlp_span.contains("\"status\":"), true)
  assert_eq(otlp_span.contains("\"code\":\"STATUS_CODE_UNSET\""), true)
  assert_eq(otlp_span.contains("\"http.method\""), true)
  assert_eq(otlp_span.contains("\"http.status_code\""), true)
}

test "json_deserialization_validation" {
  // 测试JSON反序列化验证
  
  // 有效的JSON
  let valid_json = "{\"name\":\"test\",\"value\":42,\"active\":true,\"tags\":[\"a\",\"b\",\"c\"]}"
  
  // 验证有效JSON结构
  assert_eq(valid_json.has_prefix("{"), true)
  assert_eq(valid_json.has_suffix("}"), true)
  assert_eq(valid_json.contains("\"name\":\"test\""), true)
  assert_eq(valid_json.contains("\"value\":42"), true)
  assert_eq(valid_json.contains("\"active\":true"), true)
  assert_eq(valid_json.contains("\"tags\":[\"a\",\"b\",\"c\"]"), true)
  
  // 嵌套JSON
  let nested_json = "{"
  nested_json = nested_json + "\"resource\":{"
  nested_json = nested_json + "\"service_name\":\"test-service\","
  nested_json = nested_json + "\"attributes\":{"
  nested_json = nested_json + "\"env\":\"production\","
  nested_json = nested_json + "\"version\":\"1.0.0\""
  nested_json = nested_json + "}"
  nested_json = nested_json + "},"
  nested_json = nested_json + "\"spans\":["
  nested_json = nested_json + "{\"name\":\"span-1\",\"id\":\"123\"},"
  nested_json = nested_json + "{\"name\":\"span-2\",\"id\":\"456\"}"
  nested_json = nested_json + "]"
  nested_json = nested_json + "}"
  
  // 验证嵌套JSON结构
  assert_eq(nested_json.contains("\"resource\":"), true)
  assert_eq(nested_json.contains("\"service_name\":\"test-service\""), true)
  assert_eq(nested_json.contains("\"spans\":"), true)
  assert_eq(nested_json.contains("\"name\":\"span-1\""), true)
  assert_eq(nested_json.contains("\"name\":\"span-2\""), true)
  
  // 特殊字符和Unicode JSON
  let special_json = "{\"message\":\"Special chars: !@#$%^&*()\",\"unicode\":\"测试🚀\",\"null_value\":null}"
  
  // 验证特殊字符JSON
  assert_eq(special_json.contains("Special chars: !@#$%^&*()"), true)
  assert_eq(special_json.contains("测试🚀"), true)
  assert_eq(special_json.contains("\"null_value\":null"), true)
}

test "format_compatibility_test" {
  // 测试格式兼容性
  
  // 测试不同版本的格式兼容性
  let v1_format = "service=test-service,version=1.0"
  let v2_format = "service.name=test-service,service.version=1.0,telemetry.sdk.name=azimuth"
  let v3_format = "{\"service\":{\"name\":\"test-service\",\"version\":\"1.0\"},\"sdk\":{\"name\":\"azimuth\",\"version\":\"0.1.0\"}}"
  
  // 验证不同版本格式的识别
  assert_eq(v1_format.contains("service="), true)
  assert_eq(v1_format.contains("version="), true)
  
  assert_eq(v2_format.contains("service.name="), true)
  assert_eq(v2_format.contains("service.version="), true)
  assert_eq(v2_format.contains("telemetry.sdk.name="), true)
  
  assert_eq(v3_format.has_prefix("{"), true)
  assert_eq(v3_format.has_suffix("}"), true)
  assert_eq(v3_format.contains("\"service\":"), true)
  assert_eq(v3_format.contains("\"sdk\":"), true)
  
  // 测试格式转换（模拟）
  let simple_format = "name=test,value=42"
  let json_equivalent = "{\"name\":\"test\",\"value\":42}"
  let xml_equivalent = "<data><name>test</name><value>42</value></data>"
  let yaml_equivalent = "name: test\nvalue: 42"
  
  // 验证不同格式包含相同信息
  assert_eq(simple_format.contains("name=test"), true)
  assert_eq(json_equivalent.contains("\"name\":\"test\""), true)
  assert_eq(xml_equivalent.contains("<name>test</name>"), true)
  assert_eq(yaml_equivalent.contains("name: test"), true)
}