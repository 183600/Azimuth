// Azimuth Telemetry Data Conversion and Formatting Tests
// 遥测数据转换和格式化测试，验证数据在不同格式间的转换和输出格式

// 测试1: 属性值类型转换测试
test "attribute value type conversion tests" {
  // 测试字符串属性值转换
  let string_attr = azimuth::telemetry::AttributeValue::StringValue("test_value")
  let string_to_json = azimuth::telemetry::format::attribute_to_json(string_attr)
  assert_eq(string_to_json, "\"test_value\"")
  
  let string_to_string = azimuth::telemetry::format::attribute_to_string(string_attr)
  assert_eq(string_to_string, "test_value")
  
  // 测试整数属性值转换
  let int_attr = azimuth::telemetry::AttributeValue::IntValue(42)
  let int_to_json = azimuth::telemetry::format::attribute_to_json(int_attr)
  assert_eq(int_to_json, "42")
  
  let int_to_string = azimuth::telemetry::format::attribute_to_string(int_attr)
  assert_eq(int_to_string, "42")
  
  // 测试浮点数属性值转换
  let float_attr = azimuth::telemetry::AttributeValue::FloatValue(3.14159)
  let float_to_json = azimuth::telemetry::format::attribute_to_json(float_attr)
  assert_eq(float_to_json, "3.14159")
  
  let float_to_string = azimuth::telemetry::format::attribute_to_string(float_attr)
  assert_eq(float_to_string, "3.14159")
  
  // 测试布尔属性值转换
  let bool_attr = azimuth::telemetry::AttributeValue::BoolValue(true)
  let bool_to_json = azimuth::telemetry::format::attribute_to_json(bool_attr)
  assert_eq(bool_to_json, "true")
  
  let bool_to_string = azimuth::telemetry::format::attribute_to_string(bool_attr)
  assert_eq(bool_to_string, "true")
  
  // 测试数组字符串属性值转换
  let array_string_attr = azimuth::telemetry::AttributeValue::ArrayStringValue(["a", "b", "c"])
  let array_string_to_json = azimuth::telemetry::format::attribute_to_json(array_string_attr)
  assert_eq(array_string_to_json, "[\"a\",\"b\",\"c\"]")
  
  let array_string_to_string = azimuth::telemetry::format::attribute_to_string(array_string_attr)
  assert_eq(array_string_to_string, "[a, b, c]")
  
  // 测试数组整数属性值转换
  let array_int_attr = azimuth::telemetry::AttributeValue::ArrayIntValue([1, 2, 3, 4, 5])
  let array_int_to_json = azimuth::telemetry::format::attribute_to_json(array_int_attr)
  assert_eq(array_int_to_json, "[1,2,3,4,5]")
  
  let array_int_to_string = azimuth::telemetry::format::attribute_to_string(array_int_attr)
  assert_eq(array_int_to_string, "[1, 2, 3, 4, 5]")
}

// 测试2: 度量数据序列化测试
test "metric data serialization tests" {
  // 测试计数器度量序列化
  let counter_metric = azimuth::telemetry::Metric::Counter {
    name: "http_requests_total",
    description: "Total number of HTTP requests",
    unit: "requests",
    value: 1250.0,
    attributes: [
      ("method", azimuth::telemetry::AttributeValue::StringValue("GET")),
      ("status", azimuth::telemetry::AttributeValue::StringValue("200"))
    ]
  }
  
  let counter_json = azimuth::telemetry::format::metric_to_json(counter_metric)
  assert_true(counter_json.contains("\"name\":\"http_requests_total\""))
  assert_true(counter_json.contains("\"value\":1250"))
  assert_true(counter_json.contains("\"unit\":\"requests\""))
  assert_true(counter_json.contains("\"method\":\"GET\""))
  assert_true(counter_json.contains("\"status\":\"200\""))
  
  // 测试直方图度量序列化
  let histogram_metric = azimuth::telemetry::Metric::Histogram {
    name: "request_duration_seconds",
    description: "Request duration in seconds",
    unit: "seconds",
    buckets: [
      (0.0, 0.1, 120.0),
      (0.1, 0.5, 280.0),
      (0.5, 1.0, 450.0),
      (1.0, 5.0, 130.0),
      (5.0, Infinity, 20.0)
    ],
    count: 1000.0,
    sum: 875.5
  }
  
  let histogram_json = azimuth::telemetry::format::metric_to_json(histogram_metric)
  assert_true(histogram_json.contains("\"name\":\"request_duration_seconds\""))
  assert_true(histogram_json.contains("\"count\":1000"))
  assert_true(histogram_json.contains("\"sum\":875.5"))
  assert_true(histogram_json.contains("\"buckets\""))
  
  // 测试仪表度量序列化
  let gauge_metric = azimuth::telemetry::Metric::Gauge {
    name: "memory_usage_bytes",
    description: "Current memory usage in bytes",
    unit: "bytes",
    value: 1073741824.0, // 1GB
    attributes: [
      ("instance", azimuth::telemetry::AttributeValue::StringValue("server-1")),
      ("region", azimuth::telemetry::AttributeValue::StringValue("us-west-2"))
    ]
  }
  
  let gauge_json = azimuth::telemetry::format::metric_to_json(gauge_metric)
  assert_true(gauge_json.contains("\"name\":\"memory_usage_bytes\""))
  assert_true(gauge_json.contains("\"value\":1073741824"))
  assert_true(gauge_json.contains("\"instance\":\"server-1\""))
  assert_true(gauge_json.contains("\"region\":\"us-west-2\""))
}

// 测试3: 日志记录格式化测试
test "log record formatting tests" {
  // 测试信息级别日志格式化
  let info_log = azimuth::telemetry::LogRecord {
    timestamp: 1609459200000L,
    severity: azimuth::telemetry::Severity::Info,
    severity_text: "INFO",
    body: Some("User login successful"),
    attributes: [
      ("user.id", azimuth::telemetry::AttributeValue::StringValue("user-12345")),
      ("ip.address", azimuth::telemetry::AttributeValue::StringValue("192.168.1.100")),
      ("success", azimuth::telemetry::AttributeValue::BoolValue(true))
    ],
    trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
    span_id: Some("00f067aa0ba902b7")
  }
  
  let log_json = azimuth::telemetry::format::log_to_json(info_log)
  assert_true(log_json.contains("\"timestamp\":1609459200000"))
  assert_true(log_json.contains("\"severity\":\"INFO\""))
  assert_true(log_json.contains("\"body\":\"User login successful\""))
  assert_true(log_json.contains("\"trace_id\":\"4bf92f3577b34da6a3ce929d0e0e4736\""))
  assert_true(log_json.contains("\"span_id\":\"00f067aa0ba902b7\""))
  
  // 测试错误级别日志格式化
  let error_log = azimuth::telemetry::LogRecord {
    timestamp: 1609459260000L,
    severity: azimuth::telemetry::Severity::Error,
    severity_text: "ERROR",
    body: Some("Database connection failed"),
    attributes: [
      ("error.code", azimuth::telemetry::AttributeValue::IntValue(5003)),
      ("retry.count", azimuth::telemetry::AttributeValue::IntValue(3)),
      ("database.name", azimuth::telemetry::AttributeValue::StringValue("production_db"))
    ],
    trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
    span_id: Some("00f067aa0ba902b7")
  }
  
  let error_log_json = azimuth::telemetry::format::log_to_json(error_log)
  assert_true(error_log_json.contains("\"severity\":\"ERROR\""))
  assert_true(error_log_json.contains("\"body\":\"Database connection failed\""))
  assert_true(error_log_json.contains("\"error.code\":5003"))
  assert_true(error_log_json.contains("\"retry.count\":3"))
  assert_true(error_log_json.contains("\"database.name\":\"production_db\""))
  
  // 测试日志记录转换为纯文本格式
  let log_text = azimuth::telemetry::format::log_to_text(info_log)
  assert_true(log_text.contains("2021-01-01T00:00:00Z"))
  assert_true(log_text.contains("[INFO]"))
  assert_true(log_text.contains("User login successful"))
  assert_true(log_text.contains("user.id=user-12345"))
}

// 测试4: 跨度数据格式化测试
test "span data formatting tests" {
  // 测试跨度数据JSON格式化
  let span = azimuth::telemetry::Span {
    name: "HTTP GET /api/users",
    kind: azimuth::telemetry::SpanKind::Server,
    status: azimuth::telemetry::SpanStatus::Ok,
    start_time: 1609459200000L,
    end_time: 1609459200150L,
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    parent_span_id: Some("b7ad6b7169203331"),
    attributes: [
      ("http.method", azimuth::telemetry::AttributeValue::StringValue("GET")),
      ("http.url", azimuth::telemetry::AttributeValue::StringValue("/api/users")),
      ("http.status_code", azimuth::telemetry::AttributeValue::IntValue(200))
    ],
    events: [
      {
        timestamp: 1609459200050L,
        name: "database.query",
        attributes: [
          ("db.statement", azimuth::telemetry::AttributeValue::StringValue("SELECT * FROM users")),
          ("db.duration", azimuth::telemetry::AttributeValue::FloatValue(25.5))
        ]
      }
    ],
    links: []
  }
  
  let span_json = azimuth::telemetry::format::span_to_json(span)
  assert_true(span_json.contains("\"name\":\"HTTP GET /api/users\""))
  assert_true(span_json.contains("\"kind\":\"Server\""))
  assert_true(span_json.contains("\"status\":\"Ok\""))
  assert_true(span_json.contains("\"trace_id\":\"4bf92f3577b34da6a3ce929d0e0e4736\""))
  assert_true(span_json.contains("\"span_id\":\"00f067aa0ba902b7\""))
  assert_true(span_json.contains("\"duration\":150"))
  assert_true(span_json.contains("\"http.method\":\"GET\""))
  assert_true(span_json.contains("\"http.status_code\":200"))
  assert_true(span_json.contains("\"events\""))
  
  // 测试跨度数据转换为纯文本格式
  let span_text = azimuth::telemetry::format::span_to_text(span)
  assert_true(span_text.contains("HTTP GET /api/users"))
  assert_true(span_text.contains("Server"))
  assert_true(span_text.contains("Duration: 150ms"))
  assert_true(span_text.contains("Trace ID: 4bf92f3577b34da6a3ce929d0e0e4736"))
  assert_true(span_text.contains("Span ID: 00f067aa0ba902b7"))
  assert_true(span_text.contains("database.query"))
}

// 测试5: 资源数据格式化测试
test "resource data formatting tests" {
  // 测试服务资源格式化
  let service_resource = azimuth::telemetry::Resource {
    attributes: [
      ("service.name", azimuth::telemetry::AttributeValue::StringValue("payment-service")),
      ("service.version", azimuth::telemetry::AttributeValue::StringValue("2.3.1")),
      ("service.instance.id", azimuth::telemetry::AttributeValue::StringValue("instance-abc123")),
      ("deployment.environment", azimuth::telemetry::AttributeValue::StringValue("prod"))
    ]
  }
  
  let resource_json = azimuth::telemetry::format::resource_to_json(service_resource)
  assert_true(resource_json.contains("\"service.name\":\"payment-service\""))
  assert_true(resource_json.contains("\"service.version\":\"2.3.1\""))
  assert_true(resource_json.contains("\"service.instance.id\":\"instance-abc123\""))
  assert_true(resource_json.contains("\"deployment.environment\":\"prod\""))
  
  // 测试主机资源格式化
  let host_resource = azimuth::telemetry::Resource {
    attributes: [
      ("host.name", azimuth::telemetry::AttributeValue::StringValue("web-server-01")),
      ("host.arch", azimuth::telemetry::AttributeValue::StringValue("amd64")),
      ("host.os.type", azimuth::telemetry::AttributeValue::StringValue("linux")),
      ("host.cpu.count", azimuth::telemetry::AttributeValue::IntValue(8)),
      ("host.memory.available", azimuth::telemetry::AttributeValue::IntValue(16777216000))
    ]
  }
  
  let host_resource_json = azimuth::telemetry::format::resource_to_json(host_resource)
  assert_true(host_resource_json.contains("\"host.name\":\"web-server-01\""))
  assert_true(host_resource_json.contains("\"host.arch\":\"amd64\""))
  assert_true(host_resource_json.contains("\"host.os.type\":\"linux\""))
  assert_true(host_resource_json.contains("\"host.cpu.count\":8"))
  assert_true(host_resource_json.contains("\"host.memory.available\":16777216000"))
  
  // 测试资源数据转换为纯文本格式
  let resource_text = azimuth::telemetry::format::resource_to_text(service_resource)
  assert_true(resource_text.contains("service.name=payment-service"))
  assert_true(resource_text.contains("service.version=2.3.1"))
  assert_true(resource_text.contains("service.instance.id=instance-abc123"))
  assert_true(resource_text.contains("deployment.environment=prod"))
}

// 测试6: 追踪上下文格式化测试
test "trace context formatting tests" {
  // 测试追踪上下文格式化
  let trace_context = azimuth::telemetry::TraceContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    trace_flags: 1,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE",
    is_remote: false
  }
  
  let trace_context_json = azimuth::telemetry::format::trace_context_to_json(trace_context)
  assert_true(trace_context_json.contains("\"trace_id\":\"4bf92f3577b34da6a3ce929d0e0e4736\""))
  assert_true(trace_context_json.contains("\"span_id\":\"00f067aa0ba902b7\""))
  assert_true(trace_context_json.contains("\"trace_flags\":1"))
  assert_true(trace_context_json.contains("\"trace_state\":\"rojo=00f067aa0ba902b7,congo=t61rcWkgMzE\""))
  assert_true(trace_context_json.contains("\"is_remote\":false"))
  
  // 测试追踪上下文转换为W3C traceparent格式
  let traceparent = azimuth::telemetry::format::trace_context_to_traceparent(trace_context)
  assert_eq(traceparent, "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")
  
  // 测试 baggage 上下文格式化
  let baggage_context = azimuth::telemetry::BaggageContext {
    entries: [
      {
        key: "user.id",
        value: "user-12345",
        metadata: Some({
          properties: [
            ("propagate", "true"),
            ("ttl", "3600")
          ]
        })
      },
      {
        key: "request.id",
        value: "req-abcde",
        metadata: None
      }
    ]
  }
  
  let baggage_header = azimuth::telemetry::format::baggage_to_header(baggage_context)
  assert_true(baggage_header.contains("user.id=user-12345"))
  assert_true(baggage_header.contains("request.id=req-abcde"))
  assert_true(baggage_header.contains("propagate=true"))
  assert_true(baggage_header.contains("ttl=3600"))
}

// 测试7: 时间戳格式化测试
test "timestamp formatting tests" {
  // 测试Unix时间戳转换为ISO 8601格式
  let timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  let iso8601 = azimuth::telemetry::format::timestamp_to_iso8601(timestamp)
  assert_eq(iso8601, "2021-01-01T00:00:00Z")
  
  // 测试Unix时间戳转换为RFC 3339格式
  let rfc3339 = azimuth::telemetry::format::timestamp_to_rfc3339(timestamp)
  assert_eq(rfc3339, "2021-01-01T00:00:00Z")
  
  // 测试Unix时间戳转换为人类可读格式
  let human_readable = azimuth::telemetry::format::timestamp_to_human_readable(timestamp)
  assert_eq(human_readable, "2021-01-01 00:00:00 UTC")
  
  // 测试带毫秒的时间戳格式化
  let timestamp_with_ms = 1609459200123L
  let iso8601_with_ms = azimuth::telemetry::format::timestamp_to_iso8601(timestamp_with_ms)
  assert_eq(iso8601_with_ms, "2021-01-01T00:00:00.123Z")
  
  // 测试当前时间戳格式化
  let current_timestamp = azimuth::telemetry::format::get_current_timestamp()
  assert_true(current_timestamp > 1609459200000L) // 确保时间戳是合理的
  
  let current_iso8601 = azimuth::telemetry::format::timestamp_to_iso8601(current_timestamp)
  assert_true(current_iso8601.length() > 0)
  assert_true(current_iso8601.contains("T"))
  assert_true(current_iso8601.contains("Z"))
}

// 测试8: 数据转换边界条件测试
test "data conversion edge cases tests" {
  // 测试空字符串属性值转换
  let empty_string_attr = azimuth::telemetry::AttributeValue::StringValue("")
  let empty_string_to_json = azimuth::telemetry::format::attribute_to_json(empty_string_attr)
  assert_eq(empty_string_to_json, "\"\"")
  
  let empty_string_to_string = azimuth::telemetry::format::attribute_to_string(empty_string_attr)
  assert_eq(empty_string_to_string, "")
  
  // 测试零值属性值转换
  let zero_int_attr = azimuth::telemetry::AttributeValue::IntValue(0)
  let zero_int_to_json = azimuth::telemetry::format::attribute_to_json(zero_int_attr)
  assert_eq(zero_int_to_json, "0")
  
  let zero_float_attr = azimuth::telemetry::AttributeValue::FloatValue(0.0)
  let zero_float_to_json = azimuth::telemetry::format::attribute_to_json(zero_float_attr)
  assert_eq(zero_float_to_json, "0.0")
  
  // 测试负数属性值转换
  let negative_int_attr = azimuth::telemetry::AttributeValue::IntValue(-42)
  let negative_int_to_json = azimuth::telemetry::format::attribute_to_json(negative_int_attr)
  assert_eq(negative_int_to_json, "-42")
  
  let negative_float_attr = azimuth::telemetry::AttributeValue::FloatValue(-3.14)
  let negative_float_to_json = azimuth::telemetry::format::attribute_to_json(negative_float_attr)
  assert_eq(negative_float_to_json, "-3.14")
  
  // 测试空数组属性值转换
  let empty_array_attr = azimuth::telemetry::AttributeValue::ArrayStringValue([])
  let empty_array_to_json = azimuth::telemetry::format::attribute_to_json(empty_array_attr)
  assert_eq(empty_array_to_json, "[]")
  
  let empty_array_to_string = azimuth::telemetry::format::attribute_to_string(empty_array_attr)
  assert_eq(empty_array_to_string, "[]")
  
  // 测试包含特殊字符的字符串属性值转换
  let special_chars_attr = azimuth::telemetry::AttributeValue::StringValue("Hello \"World\"!\nNew line\tTab")
  let special_chars_to_json = azimuth::telemetry::format::attribute_to_json(special_chars_attr)
  assert_true(special_chars_to_json.contains("\\\""))
  assert_true(special_chars_to_json.contains("\\n"))
  assert_true(special_chars_to_json.contains("\\t"))
  
  // 测试非常大的数值转换
  let large_int_attr = azimuth::telemetry::AttributeValue::IntValue(9223372036854775807) // Int64最大值
  let large_int_to_json = azimuth::telemetry::format::attribute_to_json(large_int_attr)
  assert_eq(large_int_to_json, "9223372036854775807")
  
  let large_float_attr = azimuth::telemetry::AttributeValue::FloatValue(1.7976931348623157e+308) // Float64最大值
  let large_float_to_json = azimuth::telemetry::format::attribute_to_json(large_float_attr)
  assert_true(large_float_to_json.contains("1.7976931348623157"))
  assert_true(large_float_to_json.contains("e+308"))
}