// 日志记录功能测试
// 测试结构化日志记录、日志级别和日志数据处理

use azimuth.telemetry.api.common
use azimuth.telemetry.api.logs

test "log_record_creation_and_validation" {
  // 测试日志记录创建和验证
  
  // 创建基本日志记录
  let basic_log = logs::LogRecord::{
    timestamp_unix_nanos: 1640995200L,
    observed_timestamp_unix_nanos: Some(1640995201L),
    severity_number: logs::Info,
    severity_text: Some("INFO"),
    body: Some("Application started successfully"),
    attributes: [
      ("service.name", common::AttributeValue::string("user-service")),
      ("service.version", common::AttributeValue::string("1.0.0"))
    ],
    trace_id: Some([for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })),
    span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte })),
    trace_flags: Some(1_byte),
    resource: Some(common::Resource::default("user-service")),
    instrumentation_scope: Some(common::InstrumentationScope::{
      name: "user-service-logger",
      version: Some("1.0.0"),
      schema_url: Some("http://example.com/schema")
    })
  }
  
  // 验证基本日志记录
  assert_eq(basic_log.timestamp_unix_nanos, 1640995200L)
  assert_eq(basic_log.observed_timestamp_unix_nanos.unwrap(), 1640995201L)
  assert_eq(basic_log.severity_number, logs::Info)
  assert_eq(basic_log.severity_text.unwrap(), "INFO")
  assert_eq(basic_log.body.unwrap(), "Application started successfully")
  assert_eq(basic_log.attributes.length(), 2)
  assert_eq(basic_log.attributes[0].0, "service.name")
  assert_eq(basic_log.attributes[0].1, common::AttributeValue::string("user-service"))
  assert_eq(basic_log.trace_id.unwrap().length(), 16)
  assert_eq(basic_log.span_id.unwrap().length(), 8)
  assert_eq(basic_log.trace_flags.unwrap(), 1_byte)
  
  // 验证资源信息
  let resource = basic_log.resource.unwrap()
  assert_eq(resource.service_name, "user-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  
  // 验证工具范围
  let scope = basic_log.instrumentation_scope.unwrap()
  assert_eq(scope.name, "user-service-logger")
  assert_eq(scope.version.unwrap(), "1.0.0")
  assert_eq(scope.schema_url.unwrap(), "http://example.com/schema")
}

test "log_severity_levels_and_hierarchy" {
  // 测试日志严重性级别和层次结构
  
  let severity_levels = [
    (logs::Trace, "TRACE", 0),
    (logs::Debug, "DEBUG", 1),
    (logs::Info, "INFO", 2),
    (logs::Warn, "WARN", 3),
    (logs::Error, "ERROR", 4),
    (logs::Fatal, "FATAL", 5)
  ]
  
  // 创建不同严重性级别的日志记录
  let mut log_records = [] : Array[logs::LogRecord]
  let mut i = 0
  
  while i < severity_levels.length() {
    let log_record = logs::LogRecord::{
      timestamp_unix_nanos: 1640995200L + i.to_int64(),
      observed_timestamp_unix_nanos: None,
      severity_number: severity_levels[i].0,
      severity_text: Some(severity_levels[i].1),
      body: Some("Log message at " + severity_levels[i].1 + " level"),
      attributes: [
        ("level", common::AttributeValue::string(severity_levels[i].1)),
        ("priority", common::AttributeValue::int(severity_levels[i].2.to_int64()))
      ],
      trace_id: None,
      span_id: None,
      trace_flags: None,
      resource: None,
      instrumentation_scope: None
    }
    
    log_records.push(log_record)
    i = i + 1
  }
  
  // 验证日志记录
  assert_eq(log_records.length(), 6)
  
  // 验证每个严重性级别
  assert_eq(log_records[0].severity_number, logs::Trace)
  assert_eq(log_records[0].severity_text.unwrap(), "TRACE")
  assert_eq(log_records[0].attributes[1].1, common::AttributeValue::int(0L))
  
  assert_eq(log_records[2].severity_number, logs::Info)
  assert_eq(log_records[2].severity_text.unwrap(), "INFO")
  assert_eq(log_records[2].attributes[1].1, common::AttributeValue::int(2L))
  
  assert_eq(log_records[5].severity_number, logs::Fatal)
  assert_eq(log_records[5].severity_text.unwrap(), "FATAL")
  assert_eq(log_records[5].attributes[1].1, common::AttributeValue::int(5L))
  
  // 验证严重性级别层次关系
  i = 0
  while i < log_records.length() - 1 {
    assert_eq(log_records[i].severity_number < log_records[i + 1].severity_number, true)
    i = i + 1
  }
}

test "structured_logging_with_attributes" {
  // 测试带属性的结构化日志
  
  // 创建复杂的日志属性
  let log_attributes = [
    ("event.name", common::AttributeValue::string("user_login")),
    ("event.category", common::AttributeValue::string("authentication")),
    ("user.id", common::AttributeValue::int(12345L)),
    ("user.name", common::AttributeValue::string("john.doe")),
    ("user.email", common::AttributeValue::string("john.doe@example.com")),
    ("user.role", common::AttributeValue::string("admin")),
    ("client.ip", common::AttributeValue::string("192.168.1.100")),
    ("client.user_agent", common::AttributeValue::string("Mozilla/5.0 (Windows NT 10.0; Win64; x64)")),
    ("http.method", common::AttributeValue::string("POST")),
    ("http.target", common::AttributeValue::string("/api/auth/login")),
    ("http.status_code", common::AttributeValue::int(200L)),
    ("http.latency_ms", common::AttributeValue::float(125.5)),
    ("database.query_count", common::AttributeValue::int(3L)),
    ("cache.hit", common::AttributeValue::bool(true)),
    ("session.id", common::AttributeValue::string("sess_abc123def456")),
    ("session.created_at", common::AttributeValue::int(1640995200L)),
    ("security.risk_score", common::AttributeValue::float(0.15)),
    ("security.mfa_verified", common::AttributeValue::bool(true)),
    ("service.name", common::AttributeValue::string("auth-service")),
    ("service.version", common::AttributeValue::string("2.1.0")),
    ("environment", common::AttributeValue::string("production"))
  ]
  
  let structured_log = logs::LogRecord::{
    timestamp_unix_nanos: 1640995200L,
    observed_timestamp_unix_nanos: Some(1640995201L),
    severity_number: logs::Info,
    severity_text: Some("INFO"),
    body: Some("User login successful"),
    attributes: log_attributes,
    trace_id: Some([for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })),
    span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte })),
    trace_flags: Some(1_byte),
    resource: Some(common::Resource::default("auth-service")),
    instrumentation_scope: Some(common::InstrumentationScope::{
      name: "auth-logger",
      version: Some("2.1.0"),
      schema_url: Some("http://example.com/auth-schema")
    })
  }
  
  // 验证结构化日志
  assert_eq(structured_log.attributes.length(), 21)
  
  // 验证特定属性
  assert_eq(structured_log.attributes[0].0, "event.name")
  assert_eq(structured_log.attributes[0].1, common::AttributeValue::string("user_login"))
  
  assert_eq(structured_log.attributes[2].0, "user.id")
  assert_eq(structured_log.attributes[2].1, common::AttributeValue::int(12345L))
  
  assert_eq(structured_log.attributes[10].0, "http.status_code")
  assert_eq(structured_log.attributes[10].1, common::AttributeValue::int(200L))
  
  assert_eq(structured_log.attributes[13].0, "cache.hit")
  assert_eq(structured_log.attributes[13].1, common::AttributeValue::bool(true))
  
  assert_eq(structured_log.attributes[20].0, "environment")
  assert_eq(structured_log.attributes[20].1, common::AttributeValue::string("production"))
  
  // 验证属性类型分布
  let mut string_attr_count = 0
  let mut int_attr_count = 0
  let mut float_attr_count = 0
  let mut bool_attr_count = 0
  let mut i = 0
  
  while i < structured_log.attributes.length() {
    match structured_log.attributes[i].1 {
      StringValue(_) => string_attr_count = string_attr_count + 1
      IntValue(_) => int_attr_count = int_attr_count + 1
      FloatValue(_) => float_attr_count = float_attr_count + 1
      BoolValue(_) => bool_attr_count = bool_attr_count + 1
    }
    i = i + 1
  }
  
  assert_eq(string_attr_count, 13)
  assert_eq(int_attr_count, 5)
  assert_eq(float_attr_count, 2)
  assert_eq(bool_attr_count, 1)
}

test "log_correlation_with_traces" {
  // 测试日志与追踪的关联
  
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte })
  
  // 创建与追踪关联的日志记录
  let correlated_logs = [
    logs::LogRecord::{
      timestamp_unix_nanos: 1640995200L,
      observed_timestamp_unix_nanos: None,
      severity_number: logs::Info,
      severity_text: Some("INFO"),
      body: Some("Starting database operation"),
      attributes: [
        ("operation", common::AttributeValue::string("query")),
        ("table", common::AttributeValue::string("users"))
      ],
      trace_id: Some(trace_id),
      span_id: Some(span_id),
      trace_flags: Some(1_byte),
      resource: None,
      instrumentation_scope: None
    },
    logs::LogRecord::{
      timestamp_unix_nanos: 1640995210L,
      observed_timestamp_unix_nanos: None,
      severity_number: logs::Debug,
      severity_text: Some("DEBUG"),
      body: Some("Executing SQL query"),
      attributes: [
        ("operation", common::AttributeValue::string("query")),
        ("sql", common::AttributeValue::string("SELECT * FROM users WHERE id = $1")),
        ("params", common::AttributeValue::array_string(["12345"]))
      ],
      trace_id: Some(trace_id),
      span_id: Some(span_id),
      trace_flags: Some(1_byte),
      resource: None,
      instrumentation_scope: None
    },
    logs::LogRecord::{
      timestamp_unix_nanos: 1640995250L,
      observed_timestamp_unix_nanos: None,
      severity_number: logs::Info,
      severity_text: Some("INFO"),
      body: Some("Database operation completed"),
      attributes: [
        ("operation", common::AttributeValue::string("query")),
        ("rows_affected", common::AttributeValue::int(1L)),
        ("execution_time_ms", common::AttributeValue::float(40.0))
      ],
      trace_id: Some(trace_id),
      span_id: Some(span_id),
      trace_flags: Some(1_byte),
      resource: None,
      instrumentation_scope: None
    }
  ]
  
  // 验证关联日志
  assert_eq(correlated_logs.length(), 3)
  
  // 验证所有日志都有相同的追踪信息
  let mut i = 0
  while i < correlated_logs.length() {
    assert_eq(correlated_logs[i].trace_id.unwrap(), trace_id)
    assert_eq(correlated_logs[i].span_id.unwrap(), span_id)
    assert_eq(correlated_logs[i].trace_flags.unwrap(), 1_byte)
    i = i + 1
  }
  
  // 验证日志时间顺序
  assert_eq(correlated_logs[0].timestamp_unix_nanos < correlated_logs[1].timestamp_unix_nanos, true)
  assert_eq(correlated_logs[1].timestamp_unix_nanos < correlated_logs[2].timestamp_unix_nanos, true)
  
  // 验证日志内容
  assert_eq(correlated_logs[0].body.unwrap(), "Starting database operation")
  assert_eq(correlated_logs[1].body.unwrap(), "Executing SQL query")
  assert_eq(correlated_logs[2].body.unwrap(), "Database operation completed")
  
  // 验证特定属性
  assert_eq(correlated_logs[1].attributes[1].1, common::AttributeValue::string("SELECT * FROM users WHERE id = $1"))
  assert_eq(correlated_logs[2].attributes[1].1, common::AttributeValue::int(1L))
  assert_eq(correlated_logs[2].attributes[2].1, common::AttributeValue::float(40.0))
}

test "error_and_exception_logging" {
  // 测试错误和异常日志
  
  let error_logs = [
    logs::LogRecord::{
      timestamp_unix_nanos: 1640995200L,
      observed_timestamp_unix_nanos: None,
      severity_number: logs::Error,
      severity_text: Some("ERROR"),
      body: Some("Database connection failed"),
      attributes: [
        ("error.type", common::AttributeValue::string("ConnectionError")),
        ("error.message", common::AttributeValue::string("Unable to connect to database server")),
        ("error.code", common::AttributeValue::string("ECONNREFUSED")),
        ("database.host", common::AttributeValue::string("db.example.com")),
        ("database.port", common::AttributeValue::int(5432L)),
        ("retry.count", common::AttributeValue::int(3L)),
        ("retry.max", common::AttributeValue::int(5L))
      ],
      trace_id: Some([for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })),
      span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte })),
      trace_flags: Some(1_byte),
      resource: None,
      instrumentation_scope: None
    },
    logs::LogRecord::{
      timestamp_unix_nanos: 1640995250L,
      observed_timestamp_unix_nanos: None,
      severity_number: logs::Fatal,
      severity_text: Some("FATAL"),
      body: Some("Application startup failed"),
      attributes: [
        ("error.type", common::AttributeValue::string("StartupError")),
        ("error.message", common::AttributeValue::string("Critical service unavailable")),
        ("error.cause", common::AttributeValue::string("Configuration error")),
        ("service.name", common::AttributeValue::string("payment-service")),
        ("exit.code", common::AttributeValue::int(1L)),
        ("graceful_shutdown", common::AttributeValue::bool(false))
      ],
      trace_id: Some([for i = 0; i < 16; i = i + 1].map(fn(_) { 2_byte })),
      span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 2_byte })),
      trace_flags: Some(1_byte),
      resource: None,
      instrumentation_scope: None
    },
    logs::LogRecord::{
      timestamp_unix_nanos: 1640995300L,
      observed_timestamp_unix_nanos: None,
      severity_number: logs::Warn,
      severity_text: Some("WARN"),
      body: Some("API rate limit approaching"),
      attributes: [
        ("warning.type", common::AttributeValue::string("RateLimitWarning")),
        ("current.rate", common::AttributeValue::float(450.0)),
        ("limit.rate", common::AttributeValue::float(500.0)),
        ("time.window", common::AttributeValue::string("1m")),
        ("client.id", common::AttributeValue::string("client-12345")),
        ("endpoint", common::AttributeValue::string("/api/process"))
      ],
      trace_id: Some([for i = 0; i < 16; i = i + 1].map(fn(_) { 3_byte })),
      span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 3_byte })),
      trace_flags: Some(1_byte),
      resource: None,
      instrumentation_scope: None
    }
  ]
  
  // 验证错误日志
  assert_eq(error_logs.length(), 3)
  
  // 验证错误日志
  assert_eq(error_logs[0].severity_number, logs::Error)
  assert_eq(error_logs[0].severity_text.unwrap(), "ERROR")
  assert_eq(error_logs[0].body.unwrap(), "Database connection failed")
  assert_eq(error_logs[0].attributes[0].1, common::AttributeValue::string("ConnectionError"))
  assert_eq(error_logs[0].attributes[5].1, common::AttributeValue::int(3L))
  
  // 验证致命错误日志
  assert_eq(error_logs[1].severity_number, logs::Fatal)
  assert_eq(error_logs[1].severity_text.unwrap(), "FATAL")
  assert_eq(error_logs[1].body.unwrap(), "Application startup failed")
  assert_eq(error_logs[1].attributes[4].1, common::AttributeValue::int(1L))
  assert_eq(error_logs[1].attributes[5].1, common::AttributeValue::bool(false))
  
  // 验证警告日志
  assert_eq(error_logs[2].severity_number, logs::Warn)
  assert_eq(error_logs[2].severity_text.unwrap(), "WARN")
  assert_eq(error_logs[2].body.unwrap(), "API rate limit approaching")
  assert_eq(error_logs[2].attributes[1].1, common::AttributeValue::float(450.0))
  assert_eq(error_logs[2].attributes[2].1, common::AttributeValue::float(500.0))
}

test "log_record_builder_pattern" {
  // 测试日志记录构建器模式
  
  // 使用构建器创建复杂日志记录
  let log_record = logs::LogRecord::builder()
    .timestamp(1640995200L)
    .severity(logs::Info)
    .body("User authentication successful")
    .with_attribute("user.id", common::AttributeValue::int(12345L))
    .with_attribute("user.name", common::AttributeValue::string("john.doe"))
    .with_attribute("auth.method", common::AttributeValue::string("password"))
    .with_attribute("client.ip", common::AttributeValue::string("192.168.1.100"))
    .with_attribute("session.id", common::AttributeValue::string("sess_abc123"))
    .build()
  
  // 验证构建的日志记录
  assert_eq(log_record.timestamp_unix_nanos, 1640995200L)
  assert_eq(log_record.severity_number, logs::Info)
  assert_eq(log_record.body.unwrap(), "User authentication successful")
  assert_eq(log_record.attributes.length(), 5)
  
  // 验证属性
  assert_eq(log_record.attributes[0].0, "user.id")
  assert_eq(log_record.attributes[0].1, common::AttributeValue::int(12345L))
  
  assert_eq(log_record.attributes[1].0, "user.name")
  assert_eq(log_record.attributes[1].1, common::AttributeValue::string("john.doe"))
  
  assert_eq(log_record.attributes[4].0, "session.id")
  assert_eq(log_record.attributes[4].1, common::AttributeValue::string("sess_abc123"))
  
  // 测试最小构建器
  let minimal_log = logs::LogRecord::builder()
    .body("Simple log message")
    .build()
  
  // 验证最小构建
  assert_eq(minimal_log.timestamp_unix_nanos, 0L)
  assert_eq(minimal_log.severity_number, logs::Info) // 默认级别
  assert_eq(minimal_log.body.unwrap(), "Simple log message")
  assert_eq(minimal_log.attributes.length(), 0)
  assert_eq(minimal_log.observed_timestamp_unix_nanos, None)
  assert_eq(minimal_log.severity_text, None)
  assert_eq(minimal_log.trace_id, None)
  assert_eq(minimal_log.span_id, None)
  assert_eq(minimal_log.trace_flags, None)
  assert_eq(minimal_log.resource, None)
  assert_eq(minimal_log.instrumentation_scope, None)
}

test "logger_convenience_methods" {
  // 测试Logger便捷方法
  
  let noop_logger_provider = logs::NoopLoggerProvider::{}
  let logger = noop_logger_provider.get_logger("test-logger", "1.0.0", "http://example.com/schema")
  
  // 测试各种便捷方法
  logger.debug("Debug message: processing request", [
    ("request.id", common::AttributeValue::string("req-123")),
    ("operation", common::AttributeValue::string("process_data"))
  ])
  
  logger.info("Info message: request completed", [
    ("request.id", common::AttributeValue::string("req-123")),
    ("duration_ms", common::AttributeValue::float(125.5))
  ])
  
  logger.warn("Warning message: high memory usage", [
    ("memory.usage", common::AttributeValue::float(85.2)),
    ("memory.limit", common::AttributeValue::float(100.0))
  ])
  
  logger.error("Error message: database connection failed", [
    ("error.type", common::AttributeValue::string("ConnectionError")),
    ("retry.count", common::AttributeValue::int(3L))
  ])
  
  logger.fatal("Fatal message: system shutdown", [
    ("shutdown.reason", common::AttributeValue::string("critical_error")),
    ("graceful", common::AttributeValue::bool(false))
  ])
  
  // 验证便捷方法不会抛出异常
  assert_eq(true, true)
  
  // 测试无属性的便捷方法
  logger.debug("Simple debug message")
  logger.info("Simple info message")
  logger.warn("Simple warning message")
  logger.error("Simple error message")
  logger.fatal("Simple fatal message")
  
  assert_eq(true, true)
  
  // 测试空属性数组的便捷方法
  let empty_attrs = [] : Array[(String, common::AttributeValue)]
  logger.debug("Debug with empty attrs", empty_attrs)
  logger.info("Info with empty attrs", empty_attrs)
  logger.warn("Warn with empty attrs", empty_attrs)
  logger.error("Error with empty attrs", empty_attrs)
  logger.fatal("Fatal with empty attrs", empty_attrs)
  
  assert_eq(true, true)
}

test "log_boundary_conditions" {
  // 测试日志边界条件
  
  // 测试极值时间戳
  let min_timestamp = -9223372036854775808L
  let max_timestamp = 9223372036854775807L
  
  let min_time_log = logs::LogRecord::{
    timestamp_unix_nanos: min_timestamp,
    observed_timestamp_unix_nanos: Some(min_timestamp),
    severity_number: logs::Debug,
    severity_text: Some("DEBUG"),
    body: Some("Log with minimum timestamp"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let max_time_log = logs::LogRecord::{
    timestamp_unix_nanos: max_timestamp,
    observed_timestamp_unix_nanos: Some(max_timestamp),
    severity_number: logs::Error,
    severity_text: Some("ERROR"),
    body: Some("Log with maximum timestamp"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证极值时间戳
  assert_eq(min_time_log.timestamp_unix_nanos, min_timestamp)
  assert_eq(max_time_log.timestamp_unix_nanos, max_timestamp)
  
  // 测试空和极长body
  let empty_body_log = logs::LogRecord::{
    timestamp_unix_nanos: 0L,
    observed_timestamp_unix_nanos: None,
    severity_number: logs::Info,
    severity_text: None,
    body: Some(""),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let long_body = "This is a very long log message " + "x" * 1000
  let long_body_log = logs::LogRecord::{
    timestamp_unix_nanos: 0L,
    observed_timestamp_unix_nanos: None,
    severity_number: logs::Warn,
    severity_text: Some("WARN"),
    body: Some(long_body),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证body边界条件
  assert_eq(empty_body_log.body.unwrap(), "")
  assert_eq(long_body_log.body.unwrap().length(), 1032) // "This is a very long log message " + 1000个"x"
  
  // 测试大量属性
  let many_attrs = [] : Array[(String, common::AttributeValue)]
  let mut i = 0
  
  while i < 100 {
    many_attrs.push(("attr" + i.to_string(), common::AttributeValue::string("value" + i.to_string())))
    i = i + 1
  }
  
  let many_attrs_log = logs::LogRecord::{
    timestamp_unix_nanos: 0L,
    observed_timestamp_unix_nanos: None,
    severity_number: logs::Info,
    severity_text: Some("INFO"),
    body: Some("Log with many attributes"),
    attributes: many_attrs,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证大量属性
  assert_eq(many_attrs_log.attributes.length(), 100)
  assert_eq(many_attrs_log.attributes[0].0, "attr0")
  assert_eq(many_attrs_log.attributes[0].1, common::AttributeValue::string("value0"))
  assert_eq(many_attrs_log.attributes[99].0, "attr99")
  assert_eq(many_attrs_log.attributes[99].1, common::AttributeValue::string("value99"))
}