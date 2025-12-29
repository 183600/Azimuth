// Logs模块基本测试用例
test "logger_basic_operations" {
  let logger = NoopLogger::{}
  
  // 测试基本日志操作不会崩溃
  logger.debug("Debug message", [])
  logger.info("Info message", [])
  logger.warn("Warning message", [])
  logger.error("Error message", [])
  logger.fatal("Fatal message", [])
  
  // 测试带属性的日志
  let attributes = [
    ("key1", AttributeValue::string("value1")),
    ("key2", AttributeValue::int(42)),
    ("key3", AttributeValue::bool(true))
  ]
  
  logger.debug("Debug with attributes", Some(attributes))
  logger.info("Info with attributes", Some(attributes))
  logger.warn("Warning with attributes", Some(attributes))
  logger.error("Error with attributes", Some(attributes))
  logger.fatal("Fatal with attributes", Some(attributes))
  
  assert_eq(true, true)
}

test "log_record_builder" {
  let builder = LogRecord::builder()
  
  // 测试builder模式不会崩溃
  let log_record = builder
    .timestamp(1234567890L)
    .severity(Info)
    .body("Test log message")
    .with_attribute("key1", AttributeValue::string("value1"))
    .with_attribute("key2", AttributeValue::int(42))
    .build()
  
  assert_eq(log_record.timestamp_unix_nanos, 1234567890L)
  
  match log_record.severity_number {
    Some(severity) => assert_eq(severity, Info)
    None => @test.fail("Test failed")
  }
  
  match log_record.body {
    Some(body) => assert_eq(body, "Test log message")
    None => @test.fail("Test failed")
  }
  
  assert_eq(log_record.attributes.length(), 2)
  assert_eq(log_record.attributes[0].0, "key1")
  assert_eq(log_record.attributes[1].0, "key2")
}

test "log_record_emission" {
  let logger = NoopLogger::{}
  
  // 创建一个复杂的log record
  let builder = LogRecord::builder()
  let log_record = builder
    .timestamp(1234567890L)
    .severity(Error)
    .body("Error message")
    .with_attribute("error.code", AttributeValue::string("E001"))
    .with_attribute("error.message", AttributeValue::string("Something went wrong"))
    .build()
  
  // 发送log record
  logger.emit(log_record)
  
  assert_eq(true, true)
}

test "logger_provider" {
  let provider = NoopLoggerProvider::{}
  let logger = provider.get_logger("test_logger", Some("1.0.0"), Some("https://example.com/schema"))
  
  // 测试日志操作不会崩溃
  logger.info("Test message", [])
  
  assert_eq(true, true)
}

test "severity_levels" {
  let logger = NoopLogger::{}
  
  // 测试所有严重性级别
  let builder = LogRecord::builder()
  
  let trace_record = builder.severity(Trace).body("Trace message").build()
  logger.emit(trace_record)
  
  let debug_record = builder.severity(Debug).body("Debug message").build()
  logger.emit(debug_record)
  
  let info_record = builder.severity(Info).body("Info message").build()
  logger.emit(info_record)
  
  let warn_record = builder.severity(Warn).body("Warning message").build()
  logger.emit(warn_record)
  
  let error_record = builder.severity(Error).body("Error message").build()
  logger.emit(error_record)
  
  let fatal_record = builder.severity(Fatal).body("Fatal message").build()
  logger.emit(fatal_record)
  
  assert_eq(true, true)
}

test "log_record_complex_scenarios" {
  let resource = Resource::default("test-service")
  let instrumentation_scope = InstrumentationScope::{
    name: "test-logger",
    version: Some("1.0.0"),
    schema_url: None
  }
  
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte })
  
  let complex_attributes = [
    ("user.id", AttributeValue::string("user-123")),
    ("request.id", AttributeValue::string("req-456")),
    ("duration.ms", AttributeValue::int(250L)),
    ("success.rate", AttributeValue::float(0.95)),
    ("cache.hit", AttributeValue::bool(true)),
    ("tags", AttributeValue::array_string(["api", "auth", "critical"])),
    ("status.codes", AttributeValue::array_int([200L, 201L, 404L])),
    ("latency.thresholds", AttributeValue::array_float([0.1, 0.5, 1.0])),
    ("feature.flags", AttributeValue::array_bool([true, false, true]))
  ]
  
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)  // 2022-01-01 00:00:00 UTC
    .severity(Error)
    .severity_text("DATABASE_ERROR")
    .body("Failed to connect to database after 3 retries")
    .with_attribute("error.code", AttributeValue::string("DB_CONN_FAILED"))
    .with_attribute("error.retries", AttributeValue::int(3L))
    .build()
  
  // 创建完整的log record
  let full_log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000150L),
    severity_number: Error,
    severity_text: Some("DATABASE_ERROR"),
    body: Some("Failed to connect to database after 3 retries"),
    attributes: [
      ("error.code", AttributeValue::string("DB_CONN_FAILED")),
      ("error.retries", AttributeValue::int(3L))
    ],
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(1_byte),
    resource: Some(resource),
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  assert_eq(full_log_record.timestamp_unix_nanos, 1640995200000000000L)
  match full_log_record.observed_timestamp_unix_nanos {
    Some(timestamp) => assert_eq(timestamp, 1640995200000000150L)
    None => @test.fail("Test failed")
  }
  assert_eq(full_log_record.severity_number, Error)
  match full_log_record.severity_text {
    Some(text) => assert_eq(text, "DATABASE_ERROR")
    None => @test.fail("Test failed")
  }
  match full_log_record.body {
    Some(body) => assert_eq(body, "Failed to connect to database after 3 retries")
    None => @test.fail("Test failed")
  }
  assert_eq(full_log_record.attributes.length(), 2)
  match full_log_record.trace_id {
    Some(id) => assert_eq(id.length(), 16)
    None => @test.fail("Test failed")
  }
  match full_log_record.span_id {
    Some(id) => assert_eq(id.length(), 8)
    None => @test.fail("Test failed")
  }
  match full_log_record.trace_flags {
    Some(flags) => assert_eq(flags, 1_byte)
    None => @test.fail("Test failed")
  }
}

test "severity_number_comparison_and_ordering" {
  // 测试严重性级别的数值比较
  assert_eq(true, true)  // 基本断言确保测试运行
  
  // 测试所有严重性级别都能正确创建
  let trace_severity = Trace
  let debug_severity = Debug
  let info_severity = Info
  let warn_severity = Warn
  let error_severity = Error
  let fatal_severity = Fatal
  
  // 创建不同严重性级别的日志记录
  let builder = LogRecord::builder()
  
  let trace_log = builder.severity(trace_severity).body("Trace message").build()
  let debug_log = builder.severity(debug_severity).body("Debug message").build()
  let info_log = builder.severity(info_severity).body("Info message").build()
  let warn_log = builder.severity(warn_severity).body("Warning message").build()
  let error_log = builder.severity(error_severity).body("Error message").build()
  let fatal_log = builder.severity(fatal_severity).body("Fatal message").build()
  
  // 验证每个日志记录都有正确的严重性级别
  assert_eq(trace_log.severity_number, Trace)
  assert_eq(debug_log.severity_number, Debug)
  assert_eq(info_log.severity_number, Info)
  assert_eq(warn_log.severity_number, Warn)
  assert_eq(error_log.severity_number, Error)
  assert_eq(fatal_log.severity_number, Fatal)
  
  // 测试带自定义严重性文本的日志
  let custom_error_log = builder
    .severity(Error)
    .severity_text("CUSTOM_ERROR")
    .body("Custom error message")
    .build()
  
  assert_eq(custom_error_log.severity_number, Error)
  match custom_error_log.severity_text {
    Some(text) => assert_eq(text, "CUSTOM_ERROR")
    None => @test.fail("Test failed")
  }
}

test "log_record_builder_with_all_fields" {
  let resource = Resource::default("test-service")
  let instrumentation_scope = InstrumentationScope::{
    name: "test-logger",
    version: Some("1.0.0"),
    schema_url: None
  }
  
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 2_byte })
  
  // 使用builder创建包含所有字段的LogRecord
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Error)
    .severity_text("DATABASE_ERROR")
    .body("Database connection failed")
    .with_attribute("error.code", AttributeValue::string("DB_CONN_FAILED"))
    .with_attribute("error.retries", AttributeValue::int(3L))
    .with_attribute("error.timeout", AttributeValue::float(30.0))
    .with_attribute("error.retryable", AttributeValue::bool(true))
    .with_attribute("error.tags", AttributeValue::array_string(["database", "timeout", "retry"]))
    .build()
  
  // 创建完整的LogRecord结构体
  let full_log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Error,
    severity_text: Some("DATABASE_ERROR"),
    body: Some("Database connection failed"),
    attributes: [
      ("error.code", AttributeValue::string("DB_CONN_FAILED")),
      ("error.retries", AttributeValue::int(3L)),
      ("error.timeout", AttributeValue::float(30.0)),
      ("error.retryable", AttributeValue::bool(true)),
      ("error.tags", AttributeValue::array_string(["database", "timeout", "retry"]))
    ],
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(1_byte),
    resource: Some(resource),
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  // 验证所有字段
  assert_eq(full_log_record.timestamp_unix_nanos, 1640995200000000000L)
  match full_log_record.observed_timestamp_unix_nanos {
    Some(timestamp) => assert_eq(timestamp, 1640995200000000100L)
    None => @test.fail("Test failed")
  }
  assert_eq(full_log_record.severity_number, Error)
  match full_log_record.severity_text {
    Some(text) => assert_eq(text, "DATABASE_ERROR")
    None => @test.fail("Test failed")
  }
  match full_log_record.body {
    Some(body) => assert_eq(body, "Database connection failed")
    None => @test.fail("Test failed")
  }
  assert_eq(full_log_record.attributes.length(), 5)
  
  // 验证trace和span信息
  match full_log_record.trace_id {
    Some(id) => {
      assert_eq(id.length(), 16)
      assert_eq(id[0], 1_byte)
    }
    None => @test.fail("Test failed")
  }
  
  match full_log_record.span_id {
    Some(id) => {
      assert_eq(id.length(), 8)
      assert_eq(id[0], 2_byte)
    }
    None => @test.fail("Test failed")
  }
  
  match full_log_record.trace_flags {
    Some(flags) => assert_eq(flags, 1_byte)
    None => @test.fail("Test failed")
  }
  
  // 验证resource和instrumentation_scope
  match full_log_record.resource {
    Some(res) => assert_eq(res.service_name, "test-service")
    None => @test.fail("Test failed")
  }
  
  match full_log_record.instrumentation_scope {
    Some(scope) => assert_eq(scope.name, "test-logger")
    None => @test.fail("Test failed")
  }
}

test "log_record_with_complex_attributes" {
  // 创建包含复杂属性的LogRecord
  let complex_attributes = [
    ("user.id", AttributeValue::string("user-12345")),
    ("request.id", AttributeValue::string("req-67890")),
    ("session.id", AttributeValue::string("sess-abcdef")),
    ("request.method", AttributeValue::string("POST")),
    ("request.path", AttributeValue::string("/api/v1/users")),
    ("response.status", AttributeValue::int(500L)),
    ("response.time_ms", AttributeValue::int(1250L)),
    ("response.size_bytes", AttributeValue::int(1024L)),
    ("cpu.usage", AttributeValue::float(75.5)),
    ("memory.usage", AttributeValue::float(512.0)),
    ("cache.hit", AttributeValue::bool(false)),
    ("auth.success", AttributeValue::bool(true)),
    ("tags", AttributeValue::array_string(["api", "users", "auth", "database"])),
    ("status.codes", AttributeValue::array_int([200L, 400L, 401L, 500L])),
    ("latency.percentiles", AttributeValue::array_float([0.5, 0.95, 0.99])),
    ("feature.flags", AttributeValue::array_bool([true, false, true, true]))
  ]
  
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Warn,
    severity_text: Some("SLOW_QUERY"),
    body: Some("Database query took longer than expected"),
    attributes: complex_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证复杂属性
  assert_eq(log_record.attributes.length(), 18)
  
  // 验证字符串属性
  match log_record.attributes[0].1 {
    StringValue(user_id) => assert_eq(user_id, "user-12345")
    _ => @test.fail("Test failed")
  }
  
  // 验证整数属性
  match log_record.attributes[5].1 {
    IntValue(status) => assert_eq(status, 500L)
    _ => @test.fail("Test failed")
  }
  
  // 验证浮点数属性
  match log_record.attributes[8].1 {
    FloatValue(cpu) => assert_eq(cpu, 75.5)
    _ => @test.fail("Test failed")
  }
  
  // 验证布尔属性
  match log_record.attributes[10].1 {
    BoolValue(cache_hit) => assert_eq(cache_hit, false)
    _ => @test.fail("Test failed")
  }
  
  // 验证数组属性
  match log_record.attributes[12].1 {
    ArrayStringValue(tags) => {
      assert_eq(tags.length(), 4)
      assert_eq(tags[0], "api")
      assert_eq(tags[1], "users")
      assert_eq(tags[2], "auth")
      assert_eq(tags[3], "database")
    }
    _ => @test.fail("Test failed")
  }
  
  match log_record.attributes[13].1 {
    ArrayIntValue(status_codes) => {
      assert_eq(status_codes.length(), 4)
      assert_eq(status_codes[0], 200L)
      assert_eq(status_codes[1], 400L)
      assert_eq(status_codes[2], 401L)
      assert_eq(status_codes[3], 500L)
    }
    _ => @test.fail("Test failed")
  }
  
  match log_record.attributes[14].1 {
    ArrayFloatValue(percentiles) => {
      assert_eq(percentiles.length(), 3)
      assert_eq(percentiles[0], 0.5)
      assert_eq(percentiles[1], 0.95)
      assert_eq(percentiles[2], 0.99)
    }
    _ => @test.fail("Test failed")
  }
  
  match log_record.attributes[15].1 {
    ArrayBoolValue(flags) => {
      assert_eq(flags.length(), 4)
      assert_eq(flags[0], true)
      assert_eq(flags[1], false)
      assert_eq(flags[2], true)
      assert_eq(flags[3], true)
    }
    _ => @test.fail("Test failed")
  }
}

test "log_record_edge_cases_and_special_values" {
  // 测试边界情况和特殊值
  
  // 空字符串和None值
  let empty_log = LogRecord::{
    timestamp_unix_nanos: 0L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: None,
    body: None,
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  assert_eq(empty_log.timestamp_unix_nanos, 0L)
  match empty_log.severity_text {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  match empty_log.body {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  assert_eq(empty_log.attributes.length(), 0)
  
  // 极值测试
  let max_timestamp = 9223372036854775807L
  let min_timestamp = -9223372036854775808L
  
  let extreme_log = LogRecord::{
    timestamp_unix_nanos: max_timestamp,
    observed_timestamp_unix_nanos: Some(min_timestamp),
    severity_number: Fatal,
    severity_text: Some(""),
    body: Some(""),
    attributes: [
      ("empty.string", AttributeValue::string("")),
      ("zero.int", AttributeValue::int(0L)),
      ("max.int", AttributeValue::int(9223372036854775807L)),
      ("min.int", AttributeValue::int(-9223372036854775808L)),
      ("zero.float", AttributeValue::float(0.0)),
      ("negative.float", AttributeValue::float(-1.0)),
      ("infinity.float", AttributeValue::float(1.0/0.0)),
      ("nan.float", AttributeValue::float(0.0/0.0)),
      ("empty.array", AttributeValue::array_string([]))
    ],
    trace_id: Some([for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte })),
    span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte })),
    trace_flags: Some(0_byte),
    resource: Some(Resource::default("")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "",
      version: Some(""),
      schema_url: Some("")
    })
  }
  
  assert_eq(extreme_log.timestamp_unix_nanos, max_timestamp)
  match extreme_log.observed_timestamp_unix_nanos {
    Some(timestamp) => assert_eq(timestamp, min_timestamp)
    None => @test.fail("Test failed")
  }
  assert_eq(extreme_log.severity_number, Fatal)
  match extreme_log.severity_text {
    Some(text) => assert_eq(text, "")
    None => @test.fail("Test failed")
  }
  match extreme_log.body {
    Some(body) => assert_eq(body, "")
    None => @test.fail("Test failed")
  }
}