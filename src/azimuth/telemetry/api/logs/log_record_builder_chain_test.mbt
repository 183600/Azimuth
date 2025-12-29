// LogRecordBuilder链式调用测试用例
// 测试LogRecordBuilder的链式调用功能和复杂场景

test "log_record_builder_basic_chaining" {
  // 测试基本的链式调用
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Info)
    .body("Basic log message")
    .with_attribute("service.name", AttributeValue::string("test-service"))
    .with_attribute("service.version", AttributeValue::string("1.0.0"))
    .build()
  
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(log_record.severity_number, Info)
  assert_eq(log_record.body, Some("Basic log message"))
  assert_eq(log_record.attributes.length(), 2)
  assert_eq(log_record.observed_timestamp_unix_nanos, None)
  assert_eq(log_record.severity_text, None)
  assert_eq(log_record.trace_id, None)
  assert_eq(log_record.span_id, None)
  assert_eq(log_record.trace_flags, None)
  assert_eq(log_record.resource, None)
  assert_eq(log_record.instrumentation_scope, None)
  
  // 验证属性值
  match log_record.attributes[0].1 {
    StringValue(s) => assert_eq(s, "test-service")
    _ => @test.fail("Expected StringValue")
  }
  
  match log_record.attributes[1].1 {
    StringValue(s) => assert_eq(s, "1.0.0")
    _ => @test.fail("Expected StringValue")
  }
}

test "log_record_builder_severity_variations" {
  // 测试不同严重程度的链式调用
  
  // Trace级别
  let trace_log = LogRecord::builder()
    .severity(Trace)
    .body("Trace message")
    .build()
  assert_eq(trace_log.severity_number, Trace)
  
  // Debug级别
  let debug_log = LogRecord::builder()
    .severity(Debug)
    .body("Debug message")
    .build()
  assert_eq(debug_log.severity_number, Debug)
  
  // Info级别
  let info_log = LogRecord::builder()
    .severity(Info)
    .body("Info message")
    .build()
  assert_eq(info_log.severity_number, Info)
  
  // Warn级别
  let warn_log = LogRecord::builder()
    .severity(Warn)
    .body("Warning message")
    .build()
  assert_eq(warn_log.severity_number, Warn)
  
  // Error级别
  let error_log = LogRecord::builder()
    .severity(Error)
    .body("Error message")
    .build()
  assert_eq(error_log.severity_number, Error)
  
  // Fatal级别
  let fatal_log = LogRecord::builder()
    .severity(Fatal)
    .body("Fatal message")
    .build()
  assert_eq(fatal_log.severity_number, Fatal)
}

test "log_record_builder_complex_attributes_chaining" {
  // 测试复杂属性的链式调用
  
  let log_record = LogRecord::builder()
    .body("Complex attributes log")
    .with_attribute("string.attr", AttributeValue::string("string.value"))
    .with_attribute("int.attr", AttributeValue::int(42L))
    .with_attribute("float.attr", AttributeValue::float(3.14))
    .with_attribute("bool.attr", AttributeValue::bool(true))
    .with_attribute("string.array.attr", AttributeValue::array_string(["a", "b", "c"]))
    .with_attribute("int.array.attr", AttributeValue::array_int([1L, 2L, 3L]))
    .with_attribute("float.array.attr", AttributeValue::array_float([1.1, 2.2, 3.3]))
    .with_attribute("bool.array.attr", AttributeValue::array_bool([true, false, true]))
    .build()
  
  assert_eq(log_record.attributes.length(), 8)
  
  // 验证各种类型的属性
  match log_record.attributes[0].1 {
    StringValue(s) => assert_eq(s, "string.value")
    _ => @test.fail("Expected StringValue")
  }
  
  match log_record.attributes[1].1 {
    IntValue(i) => assert_eq(i, 42L)
    _ => @test.fail("Expected IntValue")
  }
  
  match log_record.attributes[2].1 {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => @test.fail("Expected FloatValue")
  }
  
  match log_record.attributes[3].1 {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  match log_record.attributes[4].1 {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match log_record.attributes[5].1 {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1L)
      assert_eq(arr[1], 2L)
      assert_eq(arr[2], 3L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match log_record.attributes[6].1 {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1.1)
      assert_eq(arr[1], 2.2)
      assert_eq(arr[2], 3.3)
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match log_record.attributes[7].1 {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
}

test "log_record_builder_trace_context_chaining" {
  // 测试追踪上下文的链式调用
  
  let trace_id = Array::make(16, 1_byte)
  let span_id = Array::make(8, 2_byte)
  let trace_flags = 1_byte
  
  let log_record = LogRecord::builder()
    .body("Log with trace context")
    .timestamp(1640995200000000000L)
    .severity(Info)
    .with_attribute("service.name", AttributeValue::string("traced-service"))
    .build()
  
  // 注意：当前的LogRecordBuilder实现不直接支持设置trace context
  // 这里我们验证LogRecord结构中的相应字段
  assert_eq(log_record.trace_id, None)
  assert_eq(log_record.span_id, None)
  assert_eq(log_record.trace_flags, None)
  
  // 在实际的实现中，可能需要额外的方法来设置trace context
  // 这里我们创建一个带有trace context的LogRecord来验证结构
  let traced_log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Log with trace context"),
    attributes: [],
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(trace_flags),
    resource: None,
    instrumentation_scope: None
  }
  
  match traced_log_record.trace_id {
    Some(id) => {
      assert_eq(id.length(), 16)
      assert_eq(id[0], 1_byte)
    }
    None => @test.fail("Expected trace_id")
  }
  
  match traced_log_record.span_id {
    Some(id) => {
      assert_eq(id.length(), 8)
      assert_eq(id[0], 2_byte)
    }
    None => @test.fail("Expected span_id")
  }
  
  match traced_log_record.trace_flags {
    Some(flags) => assert_eq(flags, 1_byte)
    None => @test.fail("Expected trace_flags")
  }
}

test "log_record_builder_resource_and_instrumentation_chaining" {
  // 测试资源和仪器化范围的链式调用
  
  let resource = Resource::default("test-service")
  let instrumentation_scope = InstrumentationScope::{
    name: "test-instrumentation",
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  let log_record = LogRecord::builder()
    .body("Log with resource and instrumentation")
    .severity(Info)
    .with_attribute("user.id", AttributeValue::string("user-123"))
    .with_attribute("operation.name", AttributeValue::string("process-data"))
    .build()
  
  // 注意：当前的LogRecordBuilder实现不直接支持设置resource和instrumentation_scope
  // 这里我们验证LogRecord结构中的相应字段
  assert_eq(log_record.resource, None)
  assert_eq(log_record.instrumentation_scope, None)
  
  // 创建一个带有resource和instrumentation_scope的LogRecord来验证结构
  let full_log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000001000L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Full log record"),
    attributes: [
      ("service.name", AttributeValue::string("test-service"))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(resource),
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  match full_log_record.resource {
    Some(res) => {
      assert_eq(res.service_name, "test-service")
      assert_eq(res.telemetry_sdk_name, "azimuth")
    }
    None => @test.fail("Expected resource")
  }
  
  match full_log_record.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "test-instrumentation")
      assert_eq(scope.version, Some("1.0.0"))
      assert_eq(scope.schema_url, Some("https://opentelemetry.io/schemas/1.20.0"))
    }
    None => @test.fail("Expected instrumentation_scope")
  }
}

test "log_record_builder_edge_cases_chaining" {
  // 测试边界情况的链式调用
  
  // 空字符串和特殊值
  let edge_case_log = LogRecord::builder()
    .timestamp(0L)  // 最小时间戳
    .severity(Trace)  // 最低严重程度
    .body("")  // 空消息体
    .with_attribute("", AttributeValue::string(""))  // 空键和空值
    .with_attribute("special.chars", AttributeValue::string("!@#$%^&*()"))
    .with_attribute("unicode", AttributeValue::string("测试🚀"))
    .with_attribute("zero.int", AttributeValue::int(0L))
    .with_attribute("zero.float", AttributeValue::float(0.0))
    .with_attribute("false.bool", AttributeValue::bool(false))
    .with_attribute("empty.array", AttributeValue::array_string([]))
    .build()
  
  assert_eq(edge_case_log.timestamp_unix_nanos, 0L)
  assert_eq(edge_case_log.severity_number, Trace)
  assert_eq(edge_case_log.body, Some(""))
  assert_eq(edge_case_log.attributes.length(), 8)
  
  // 验证边界值
  match edge_case_log.attributes[0].1 {
    StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Expected StringValue")
  }
  
  match edge_case_log.attributes[4].1 {
    IntValue(i) => assert_eq(i, 0L)
    _ => @test.fail("Expected IntValue")
  }
  
  match edge_case_log.attributes[5].1 {
    FloatValue(f) => assert_eq(f, 0.0)
    _ => @test.fail("Expected FloatValue")
  }
  
  match edge_case_log.attributes[6].1 {
    BoolValue(b) => assert_eq(b, false)
    _ => @test.fail("Expected BoolValue")
  }
  
  match edge_case_log.attributes[7].1 {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  // 极值测试
  let extreme_log = LogRecord::builder()
    .timestamp(9223372036854775807L)  // Int64最大值
    .severity(Fatal)  // 最高严重程度
    .body("Extreme values test")
    .with_attribute("max.int", AttributeValue::int(9223372036854775807L))
    .with_attribute("min.int", AttributeValue::int(-9223372036854775808L))
    .with_attribute("max.float", AttributeValue::float(1.7976931348623157e+308))
    .with_attribute("min.float", AttributeValue::float(-1.7976931348623157e+308))
    .with_attribute("inf.float", AttributeValue::float(1.0/0.0))
    .with_attribute("neg.inf.float", AttributeValue::float(-1.0/0.0))
    .with_attribute("nan.float", AttributeValue::float(0.0/0.0))
    .build()
  
  assert_eq(extreme_log.timestamp_unix_nanos, 9223372036854775807L)
  assert_eq(extreme_log.severity_number, Fatal)
  assert_eq(extreme_log.body, Some("Extreme values test"))
  assert_eq(extreme_log.attributes.length(), 7)
}

test "log_record_builder_observability_chaining" {
  // 测试可观测性相关的链式调用
  
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Info)
    .body("Observability log")
    .with_attribute("service.name", AttributeValue::string("observability-service"))
    .with_attribute("service.version", AttributeValue::string("2.1.0"))
    .with_attribute("service.namespace", AttributeValue::string("production"))
    .with_attribute("service.instance.id", AttributeValue::string("instance-456"))
    .with_attribute("deployment.environment", AttributeValue::string("prod"))
    .with_attribute("host.name", AttributeValue::string("prod-server-01"))
    .with_attribute("k8s.pod.name", AttributeValue::string("observability-pod-abc123"))
    .with_attribute("k8s.namespace", AttributeValue::string("default"))
    .with_attribute("process.pid", AttributeValue::int(12345L))
    .with_attribute("process.executable.name", AttributeValue::string("observability-app"))
    .with_attribute("process.command", AttributeValue::string("./observability-app --config prod.yaml"))
    .with_attribute("telemetry.sdk.name", AttributeValue::string("azimuth"))
    .with_attribute("telemetry.sdk.version", AttributeValue::string("0.1.0"))
    .with_attribute("telemetry.sdk.language", AttributeValue::string("moonbit"))
    .build()
  
  assert_eq(log_record.attributes.length(), 15)
  
  // 验证可观测性属性
  match log_record.attributes[0].1 {
    StringValue(s) => assert_eq(s, "observability-service")
    _ => @test.fail("Expected StringValue")
  }
  
  match log_record.attributes[8].1 {
    IntValue(i) => assert_eq(i, 12345L)
    _ => @test.fail("Expected IntValue")
  }
  
  match log_record.attributes[14].1 {
    StringValue(s) => assert_eq(s, "moonbit")
    _ => @test.fail("Expected StringValue")
  }
}

test "log_record_builder_business_logging_chaining" {
  // 测试业务日志的链式调用
  
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Info)
    .body("User login successful")
    .with_attribute("event.name", AttributeValue::string("user.login"))
    .with_attribute("event.category", AttributeValue::string("authentication"))
    .with_attribute("user.id", AttributeValue::string("user-789"))
    .with_attribute("user.name", AttributeValue::string("张三"))
    .with_attribute("user.email", AttributeValue::string("zhangsan@example.com"))
    .with_attribute("user.role", AttributeValue::string("admin"))
    .with_attribute("authentication.method", AttributeValue::string("password"))
    .with_attribute("authentication.provider", AttributeValue::string("local"))
    .with_attribute("client.ip", AttributeValue::string("192.168.1.100"))
    .with_attribute("client.user_agent", AttributeValue::string("Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    .with_attribute("session.id", AttributeValue::string("session-abc123"))
    .with_attribute("request.id", AttributeValue::string("req-def456"))
    .with_attribute("response.time.ms", AttributeValue::int(150L))
    .with_attribute("success", AttributeValue::bool(true))
    .with_attribute("security.level", AttributeValue::string("high"))
    .with_attribute("compliance.required", AttributeValue::bool(true))
    .build()
  
  assert_eq(log_record.body, Some("User login successful"))
  assert_eq(log_record.attributes.length(), 17)
  
  // 验证业务日志属性
  match log_record.attributes[0].1 {
    StringValue(s) => assert_eq(s, "user.login")
    _ => @test.fail("Expected StringValue")
  }
  
  match log_record.attributes[2].1 {
    StringValue(s) => assert_eq(s, "user-789")
    _ => @test.fail("Expected StringValue")
  }
  
  match log_record.attributes[14].1 {
    IntValue(i) => assert_eq(i, 150L)
    _ => @test.fail("Expected IntValue")
  }
  
  match log_record.attributes[15].1 {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
}

test "log_record_builder_error_logging_chaining" {
  // 测试错误日志的链式调用
  
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Error)
    .body("Database connection failed")
    .with_attribute("error.type", AttributeValue::string("ConnectionException"))
    .with_attribute("error.message", AttributeValue::string("Unable to connect to database: Connection timeout"))
    .with_attribute("error.code", AttributeValue::string("DB_CONN_TIMEOUT"))
    .with_attribute("error.stack_trace", AttributeValue::string("at Database.connect(db_config.go:45)\nat Service.initialize(service.go:23)\nat main.main(main.go:10)"))
    .with_attribute("component.name", AttributeValue::string("database-client"))
    .with_attribute("component.version", AttributeValue::string("1.2.3"))
    .with_attribute("database.system", AttributeValue::string("postgresql"))
    .with_attribute("database.host", AttributeValue::string("db.example.com"))
    .with_attribute("database.port", AttributeValue::int(5432L))
    .with_attribute("database.name", AttributeValue::string("production_db"))
    .with_attribute("connection.timeout.ms", AttributeValue::int(30000L))
    .with_attribute("retry.count", AttributeValue::int(3L))
    .with_attribute("retry.max.count", AttributeValue::int(5L))
    .with_attribute("impact.severity", AttributeValue::string("high"))
    .with_attribute("requires.escalation", AttributeValue::bool(true))
    .with_attribute("alert.sent", AttributeValue::bool(false))
    .build()
  
  assert_eq(log_record.severity_number, Error)
  assert_eq(log_record.body, Some("Database connection failed"))
  assert_eq(log_record.attributes.length(), 17)
  
  // 验证错误日志属性
  match log_record.attributes[0].1 {
    StringValue(s) => assert_eq(s, "ConnectionException")
    _ => @test.fail("Expected StringValue")
  }
  
  match log_record.attributes[9].1 {
    IntValue(i) => assert_eq(i, 5432L)
    _ => @test.fail("Expected IntValue")
  }
  
  match log_record.attributes[15].1 {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  match log_record.attributes[16].1 {
    BoolValue(b) => assert_eq(b, false)
    _ => @test.fail("Expected BoolValue")
  }
}

test "log_record_builder_performance_logging_chaining" {
  // 测试性能日志的链式调用
  
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Info)
    .body("Performance metrics collected")
    .with_attribute("metric.name", AttributeValue::string("http.request.duration"))
    .with_attribute("metric.type", AttributeValue::string("histogram"))
    .with_attribute("metric.unit", AttributeValue::string("ms"))
    .with_attribute("metric.value", AttributeValue::float(125.5))
    .with_attribute("metric.count", AttributeValue::int(1000L))
    .with_attribute("metric.sum", AttributeValue::float(125500.0))
    .with_attribute("metric.min", AttributeValue::float(10.0))
    .with_attribute("metric.max", AttributeValue::float(500.0))
    .with_attribute("metric.avg", AttributeValue::float(125.5))
    .with_attribute("metric.p50", AttributeValue::float(100.0))
    .with_attribute("metric.p90", AttributeValue::float(200.0))
    .with_attribute("metric.p95", AttributeValue::float(250.0))
    .with_attribute("metric.p99", AttributeValue::float(400.0))
    .with_attribute("http.method", AttributeValue::string("GET"))
    .with_attribute("http.route", AttributeValue::string("/api/users"))
    .with_attribute("http.status_code", AttributeValue::int(200L))
    .with_attribute("service.name", AttributeValue::string("api-gateway"))
    .build()
  
  assert_eq(log_record.body, Some("Performance metrics collected"))
  assert_eq(log_record.attributes.length(), 18)
  
  // 验证性能日志属性
  match log_record.attributes[0].1 {
    StringValue(s) => assert_eq(s, "http.request.duration")
    _ => @test.fail("Expected StringValue")
  }
  
  match log_record.attributes[3].1 {
    FloatValue(f) => assert_eq(f, 125.5)
    _ => @test.fail("Expected FloatValue")
  }
  
  match log_record.attributes[4].1 {
    IntValue(i) => assert_eq(i, 1000L)
    _ => @test.fail("Expected IntValue")
  }
  
  match log_record.attributes[17].1 {
    StringValue(s) => assert_eq(s, "api-gateway")
    _ => @test.fail("Expected StringValue")
  }
}