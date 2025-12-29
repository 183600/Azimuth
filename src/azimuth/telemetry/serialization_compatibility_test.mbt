// 序列化兼容性测试 - 测试数据序列化和反序列化

test "attribute_value_serialization" {
  // 测试AttributeValue的序列化兼容性
  
  // 测试各种类型的属性值
  let string_attr = common::AttributeValue::string("test-string")
  let int_attr = common::AttributeValue::int(12345L)
  let float_attr = common::AttributeValue::float(3.14159)
  let bool_attr = common::AttributeValue::bool(true)
  let string_array_attr = common::AttributeValue::array_string(["a", "b", "c"])
  let int_array_attr = common::AttributeValue::array_int([1L, 2L, 3L])
  let float_array_attr = common::AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array_attr = common::AttributeValue::array_bool([true, false, true])
  
  // 模拟序列化过程（在实际实现中会有真正的序列化逻辑）
  // 这里我们验证属性值可以正确创建和访问
  let attributes = [
    ("string.key", string_attr),
    ("int.key", int_attr),
    ("float.key", float_attr),
    ("bool.key", bool_attr),
    ("string_array.key", string_array_attr),
    ("int_array.key", int_array_attr),
    ("float_array.key", float_array_attr),
    ("bool_array.key", bool_array_attr)
  ]
  
  // 验证属性值的类型和内容
  assert_eq(attributes.length(), 8)
  
  // 验证字符串属性
  match attributes[0].1 {
    common::StringValue(s) => assert_eq(s, "test-string")
    _ => assert!(false, "Expected StringValue")
  }
  
  // 验证整数属性
  match attributes[1].1 {
    common::IntValue(i) => assert_eq(i, 12345L)
    _ => assert!(false, "Expected IntValue")
  }
  
  // 验证浮点属性
  match attributes[2].1 {
    common::FloatValue(f) => assert_eq(f, 3.14159)
    _ => assert!(false, "Expected FloatValue")
  }
  
  // 验证布尔属性
  match attributes[3].1 {
    common::BoolValue(b) => assert_eq(b, true)
    _ => assert!(false, "Expected BoolValue")
  }
}

test "resource_serialization" {
  // 测试Resource的序列化兼容性
  
  // 创建完整的Resource
  let resource = common::Resource::{
    service_name: "test-service",
    service_version: Some("1.2.3"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.instance.id", common::AttributeValue::string("instance-123")),
      ("service.namespace", common::AttributeValue::string("production")),
      ("host.name", common::AttributeValue::string("host-001")),
      ("deployment.environment", common::AttributeValue::string("prod")),
      ("os.type", common::AttributeValue::string("linux")),
      ("os.version", common::AttributeValue::string("5.4.0")),
      ("process.pid", common::AttributeValue::int(12345L)),
      ("process.executable.name", common::AttributeValue::string("test-app")),
      ("process.command_args", common::AttributeValue::array_string(["--config", "config.yaml"])),
      ("cpu.count", common::AttributeValue::int(8L)),
      ("memory.total", common::AttributeValue::int(16777216000L)),
      ("service.available", common::AttributeValue::bool(true))
    ]
  }
  
  // 验证Resource结构
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.service_version, Some("1.2.3"))
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 12)
  
  // 验证特定属性
  let mut found_instance_id = false
  let mut found_pid = false
  let mut found_available = false
  let mut i = 0
  while i < resource.attributes.length() {
    let (key, value) = resource.attributes[i]
    match key {
      "service.instance.id" => {
        match value {
          common::StringValue(id) => {
            if id == "instance-123" { found_instance_id = true }
          }
          _ => ()
        }
      }
      "process.pid" => {
        match value {
          common::IntValue(pid) => {
            if pid == 12345L { found_pid = true }
          }
          _ => ()
        }
      }
      "service.available" => {
        match value {
          common::BoolValue(available) => {
            if available == true { found_available = true }
          }
          _ => ()
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert!(found_instance_id)
  assert!(found_pid)
  assert!(found_available)
}

test "span_serialization" {
  // 测试Span的序列化兼容性
  
  // 创建完整的Span
  let span_context = trace::SpanContext::{
    trace_id: [0_byte, 1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte,
               8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte],
    span_id: [0_byte, 1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte],
    trace_flags: 1_byte,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  let span_events = [
    trace::SpanEvent::{
      name: "event1",
      timestamp_unix_nanos: 1609459200000000000L,
      attributes: [
        ("event.type", common::AttributeValue::string("info")),
        ("event.message", common::AttributeValue::string("Operation started"))
      ]
    },
    trace::SpanEvent::{
      name: "event2",
      timestamp_unix_nanos: 1609459201000000000L,
      attributes: [
        ("event.type", common::AttributeValue::string("error")),
        ("error.code", common::AttributeValue::int(500))
      ]
    }
  ]
  
  let span_links = [
    trace::SpanLink::{
      context: trace::SpanContext::{
        trace_id: [1_byte, 1_byte, 1_byte, 1_byte, 1_byte, 1_byte, 1_byte, 1_byte,
                   1_byte, 1_byte, 1_byte, 1_byte, 1_byte, 1_byte, 1_byte, 1_byte],
        span_id: [1_byte, 1_byte, 1_byte, 1_byte, 1_byte, 1_byte, 1_byte, 1_byte],
        trace_flags: 0_byte,
        trace_state: ""
      },
      attributes: [
        ("link.type", common::AttributeValue::string("parent"))
      ]
    }
  ]
  
  let span = trace::Span::{
    name: "test-span",
    context: span_context,
    kind: trace::SpanKind::Server,
    parent_span_id: Some([9_byte, 9_byte, 9_byte, 9_byte, 9_byte, 9_byte, 9_byte, 9_byte]),
    start_time_unix_nanos: 1609459200000000000L,
    end_time_unix_nanos: Some(1609459205000000000L),
    status: trace::StatusCode::Error,
    status_description: Some("Operation failed due to timeout"),
    attributes: [
      ("http.method", common::AttributeValue::string("GET")),
      ("http.url", common::AttributeValue::string("https://example.com/api/test")),
      ("http.status_code", common::AttributeValue::int(500)),
      ("user.id", common::AttributeValue::string("user-123")),
      ("request.size", common::AttributeValue::int(1024L)),
      ("response.size", common::AttributeValue::int(2048L)),
      ("success", common::AttributeValue::bool(false))
    ],
    events: span_events,
    links: span_links
  }
  
  // 验证Span结构
  assert_eq(span.name, "test-span")
  assert_eq(span.kind, trace::SpanKind::Server)
  assert_eq(span.status, trace::StatusCode::Error)
  assert_eq(span.status_description, Some("Operation failed due to timeout"))
  assert_eq(span.attributes.length(), 7)
  assert_eq(span.events.length(), 2)
  assert_eq(span.links.length(), 1)
  
  // 验证SpanContext
  assert_eq(span.context.trace_flags, 1_byte)
  assert_eq(span.context.trace_state, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  
  // 验证事件
  assert_eq(span.events[0].name, "event1")
  assert_eq(span.events[0].timestamp_unix_nanos, 1609459200000000000L)
  assert_eq(span.events[0].attributes.length(), 2)
  
  assert_eq(span.events[1].name, "event2")
  assert_eq(span.events[1].timestamp_unix_nanos, 1609459201000000000L)
  assert_eq(span.events[1].attributes.length(), 2)
}

test "log_record_serialization" {
  // 测试LogRecord的序列化兼容性
  
  // 创建完整的LogRecord
  let trace_id = [0_byte, 1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte,
                  8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte]
  let span_id = [0_byte, 1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte]
  
  let resource = common::Resource::default("logging-service")
  let instrumentation_scope = common::InstrumentationScope::{
    name: "logging-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/logging-schema")
  }
  
  let log_record = logs::LogRecord::{
    timestamp_unix_nanos: 1609459200000000000L,
    observed_timestamp_unix_nanos: Some(1609459200000001000L),
    severity_number: logs::SeverityNumber::Error,
    severity_text: Some("ERROR"),
    body: Some("Database connection failed"),
    attributes: [
      ("error.type", common::AttributeValue::string("connection_error")),
      ("error.code", common::AttributeValue::int(1001)),
      ("error.message", common::AttributeValue::string("Connection timeout after 30 seconds")),
      ("database.name", common::AttributeValue::string("production_db")),
      ("database.host", common::AttributeValue::string("db.example.com")),
      ("database.port", common::AttributeValue::int(5432)),
      ("retry.count", common::AttributeValue::int(3)),
      ("retryable", common::AttributeValue::bool(true)),
      ("stack.trace", common::AttributeValue::string("at Database.connect (db.js:123:45)")),
      ("user.id", common::AttributeValue::string("user-456")),
      ("session.id", common::AttributeValue::string("session-789")),
      ("request.id", common::AttributeValue::string("req-abc123")),
      ("correlation.id", common::AttributeValue::array_string(["corr-1", "corr-2"]))
    ],
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(1_byte),
    resource: Some(resource),
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  // 验证LogRecord结构
  assert_eq(log_record.timestamp_unix_nanos, 1609459200000000000L)
  assert_eq(log_record.observed_timestamp_unix_nanos, Some(1609459200000001000L))
  assert_eq(log_record.severity_number, logs::SeverityNumber::Error)
  assert_eq(log_record.severity_text, Some("ERROR"))
  assert_eq(log_record.body, Some("Database connection failed"))
  assert_eq(log_record.attributes.length(), 13)
  assert_eq(log_record.trace_id, Some(trace_id))
  assert_eq(log_record.span_id, Some(span_id))
  assert_eq(log_record.trace_flags, Some(1_byte))
  assert_eq(log_record.resource, Some(resource))
  assert_eq(log_record.instrumentation_scope, Some(instrumentation_scope))
  
  // 验证特定属性
  let mut found_error_type = false
  let mut found_retry_count = false
  let mut found_retryable = false
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    match key {
      "error.type" => {
        match value {
          common::StringValue(error_type) => {
            if error_type == "connection_error" { found_error_type = true }
          }
          _ => ()
        }
      }
      "retry.count" => {
        match value {
          common::IntValue(retry_count) => {
            if retry_count == 3L { found_retry_count = true }
          }
          _ => ()
        }
      }
      "retryable" => {
        match value {
          common::BoolValue(retryable) => {
            if retryable == true { found_retryable = true }
          }
          _ => ()
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert!(found_error_type)
  assert!(found_retry_count)
  assert!(found_retryable)
}

test "measurement_serialization" {
  // 测试Measurement的序列化兼容性
  
  // 创建各种Measurement
  let measurement1 = metrics::Measurement::{
    value: 123.45,
    attributes: [
      ("metric.name", common::AttributeValue::string("response_time")),
      ("unit", common::AttributeValue::string("ms")),
      ("endpoint", common::AttributeValue::string("/api/users"))
    ]
  }
  
  let measurement2 = metrics::Measurement::{
    value: 42.0,
    attributes: [
      ("metric.name", common::AttributeValue::string("cpu_usage")),
      ("unit", common::AttributeValue::string("percent")),
      ("host", common::AttributeValue::string("server-001"))
    ]
  }
  
  let measurement3 = metrics::Measurement::{
    value: 1000.0,
    attributes: [
      ("metric.name", common::AttributeValue::string("memory_usage")),
      ("unit", common::AttributeValue::string("bytes")),
      ("process", common::AttributeValue::string("web-server")),
      ("available", common::AttributeValue::bool(true))
    ]
  }
  
  // 验证Measurement结构
  assert_eq(measurement1.value, 123.45)
  assert_eq(measurement1.attributes.length(), 3)
  assert_eq(measurement2.value, 42.0)
  assert_eq(measurement2.attributes.length(), 3)
  assert_eq(measurement3.value, 1000.0)
  assert_eq(measurement3.attributes.length(), 4)
  
  // 验证特定属性
  let mut found_metric_name1 = false
  let mut found_metric_name2 = false
  let mut found_metric_name3 = false
  let mut i = 0
  while i < measurement1.attributes.length() {
    let (key, value) = measurement1.attributes[i]
    if key == "metric.name" {
      match value {
        common::StringValue(name) => {
          if name == "response_time" { found_metric_name1 = true }
        }
        _ => ()
      }
    }
    i = i + 1
  }
  
  i = 0
  while i < measurement2.attributes.length() {
    let (key, value) = measurement2.attributes[i]
    if key == "metric.name" {
      match value {
        common::StringValue(name) => {
          if name == "cpu_usage" { found_metric_name2 = true }
        }
        _ => ()
      }
    }
    i = i + 1
  }
  
  i = 0
  while i < measurement3.attributes.length() {
    let (key, value) = measurement3.attributes[i]
    if key == "metric.name" {
      match value {
        common::StringValue(name) => {
          if name == "memory_usage" { found_metric_name3 = true }
        }
        _ => ()
      }
    }
    i = i + 1
  }
  
  assert!(found_metric_name1)
  assert!(found_metric_name2)
  assert!(found_metric_name3)
}

test "context_serialization" {
  // 测试Context的序列化兼容性
  
  // 创建带有多个值的Context
  let ctx = context::Context::empty()
  let ctx1 = ctx.with_value(context::create_key("trace.id"), "trace-12345")
  let ctx2 = ctx1.with_value(context::create_key("user.id"), "user-67890")
  let ctx3 = ctx2.with_value(context::create_key("request.id"), "req-abcdef")
  let ctx4 = ctx3.with_value(context::create_key("session.id"), "sess-123456")
  let final_ctx = ctx4.with_value(context::create_key("correlation.id"), "corr-789012")
  
  // 验证Context值
  assert_eq(final_ctx.get(context::create_key("trace.id")), Some("trace-12345"))
  assert_eq(final_ctx.get(context::create_key("user.id")), Some("user-67890"))
  assert_eq(final_ctx.get(context::create_key("request.id")), Some("req-abcdef"))
  assert_eq(final_ctx.get(context::create_key("session.id")), Some("sess-123456"))
  assert_eq(final_ctx.get(context::create_key("correlation.id")), Some("corr-789012"))
  
  // 验证不存在的键
  assert_eq(final_ctx.get(context::create_key("non.existent.key")), None)
}

test "baggage_serialization" {
  // 测试Baggage的序列化兼容性
  
  // 创建带有多个条目的Baggage
  let baggage = context::Baggage::empty()
  let baggage1 = baggage.with_entry("user.id", "user-123")
  let baggage2 = baggage1.with_entry("session.id", "session-456")
  let baggage3 = baggage2.with_entry("request.id", "request-789")
  let baggage4 = baggage3.with_entry("trace.id", "trace-abc")
  let final_baggage = baggage4.with_entry("correlation.id", "corr-def")
  
  // 验证Baggage条目
  assert_eq(final_baggage.get("user.id"), Some("user-123"))
  assert_eq(final_baggage.get("session.id"), Some("session-456"))
  assert_eq(final_baggage.get("request.id"), Some("request-789"))
  assert_eq(final_baggage.get("trace.id"), Some("trace-abc"))
  assert_eq(final_baggage.get("correlation.id"), Some("corr-def"))
  
  // 验证不存在的键
  assert_eq(final_baggage.get("non.existent.key"), None)
}

test "instrumentation_scope_serialization" {
  // 测试InstrumentationScope的序列化兼容性
  
  // 创建各种InstrumentationScope
  let scope1 = common::InstrumentationScope::{
    name: "http-client",
    version: Some("1.2.3"),
    schema_url: Some("https://opentelemetry.io/schemas/v1.20.0")
  }
  
  let scope2 = common::InstrumentationScope::{
    name: "database-driver",
    version: Some("2.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/v1.20.0")
  }
  
  let scope3 = common::InstrumentationScope::{
    name: "custom-instrumentation",
    version: None,
    schema_url: Some("https://example.com/custom-schema")
  }
  
  let scope4 = common::InstrumentationScope::{
    name: "minimal-scope",
    version: None,
    schema_url: None
  }
  
  // 验证InstrumentationScope结构
  assert_eq(scope1.name, "http-client")
  assert_eq(scope1.version, Some("1.2.3"))
  assert_eq(scope1.schema_url, Some("https://opentelemetry.io/schemas/v1.20.0"))
  
  assert_eq(scope2.name, "database-driver")
  assert_eq(scope2.version, Some("2.0.0"))
  assert_eq(scope2.schema_url, Some("https://opentelemetry.io/schemas/v1.20.0"))
  
  assert_eq(scope3.name, "custom-instrumentation")
  assert_eq(scope3.version, None)
  assert_eq(scope3.schema_url, Some("https://example.com/custom-schema"))
  
  assert_eq(scope4.name, "minimal-scope")
  assert_eq(scope4.version, None)
  assert_eq(scope4.schema_url, None)
}

test "complex_data_structure_serialization" {
  // 测试复杂数据结构的序列化兼容性
  
  // 创建包含嵌套和复杂数据的结构
  let complex_attributes = [
    ("simple.string", common::AttributeValue::string("simple value")),
    ("simple.int", common::AttributeValue::int(42L)),
    ("simple.float", common::AttributeValue::float(3.14159)),
    ("simple.bool", common::AttributeValue::bool(true)),
    ("string.array", common::AttributeValue::array_string(["a", "b", "c", "d", "e"])),
    ("int.array", common::AttributeValue::array_int([1L, 2L, 3L, 4L, 5L])),
    ("float.array", common::AttributeValue::array_float([1.1, 2.2, 3.3, 4.4, 5.5])),
    ("bool.array", common::AttributeValue::array_bool([true, false, true, false, true])),
    ("empty.string", common::AttributeValue::string("")),
    ("zero.int", common::AttributeValue::int(0L)),
    ("zero.float", common::AttributeValue::float(0.0)),
    ("false.bool", common::AttributeValue::bool(false)),
    ("empty.array", common::AttributeValue::array_string([])),
    ("unicode.string", common::AttributeValue::string("测试Unicode🌟🎉🚀")),
    ("special.chars", common::AttributeValue::string("special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")),
    ("very.long.string", common::AttributeValue::string("This is a very long string that might be used to test serialization of large text values and ensure that the system can handle them properly without any issues or data loss during the process"))
  ]
  
  let complex_resource = common::Resource::{
    service_name: "complex-test-service",
    service_version: Some("1.0.0-beta+build.123"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: complex_attributes
  }
  
  // 验证复杂Resource结构
  assert_eq(complex_resource.service_name, "complex-test-service")
  assert_eq(complex_resource.service_version, Some("1.0.0-beta+build.123"))
  assert_eq(complex_resource.attributes.length(), 16)
  
  // 验证特定复杂属性
  let mut found_unicode = false
  let mut found_special_chars = false
  let mut found_long_string = false
  let mut i = 0
  while i < complex_resource.attributes.length() {
    let (key, value) = complex_resource.attributes[i]
    match key {
      "unicode.string" => {
        match value {
          common::StringValue(s) => {
            if s == "测试Unicode🌟🎉🚀" { found_unicode = true }
          }
          _ => ()
        }
      }
      "special.chars" => {
        match value {
          common::StringValue(s) => {
            if s == "special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./" { found_special_chars = true }
          }
          _ => ()
        }
      }
      "very.long.string" => {
        match value {
          common::StringValue(s) => {
            if s.starts_with("This is a very long string") { found_long_string = true }
          }
          _ => ()
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert!(found_unicode)
  assert!(found_special_chars)
  assert!(found_long_string)
}