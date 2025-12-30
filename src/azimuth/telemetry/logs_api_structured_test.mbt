// 结构化日志API测试用例
// 测试Azimuth Telemetry Logs API的结构化日志功能

test "log_record_builder_pattern" {
  // 测试日志记录构建器模式
  
  // 使用构建器创建基本日志记录
  let log_record = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("User login successful")
    .with_attribute("user.id", common::AttributeValue::string("user-12345"))
    .with_attribute("user.email", common::AttributeValue::string("user@example.com"))
    .with_attribute("ip.address", common::AttributeValue::string("192.168.1.100"))
    .build()
  
  // 验证基本字段
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  match log_record.severity_number {
    logs::Info => assert_eq(true, true)
    _ => assert_eq(false, true, "Expected Info severity")
  }
  
  match log_record.body {
    Some(body_text) => assert_eq(body_text, "User login successful")
    None => assert_eq(false, true, "Expected body to be Some")
  }
  
  // 验证属性
  assert_eq(log_record.attributes.length(), 3)
  
  let mut found_user_id = false
  let mut found_user_email = false
  let mut found_ip_address = false
  
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    
    if key == "user.id" {
      match value {
        common::StringValue(user_id) => {
          assert_eq(user_id, "user-12345")
          found_user_id = true
        }
        _ => assert_eq(false, true, "Expected StringValue for user.id")
      }
    }
    
    if key == "user.email" {
      match value {
        common::StringValue(email) => {
          assert_eq(email, "user@example.com")
          found_user_email = true
        }
        _ => assert_eq(false, true, "Expected StringValue for user.email")
      }
    }
    
    if key == "ip.address" {
      match value {
        common::StringValue(ip) => {
          assert_eq(ip, "192.168.1.100")
          found_ip_address = true
        }
        _ => assert_eq(false, true, "Expected StringValue for ip.address")
      }
    }
    
    i = i + 1
  }
  
  assert_eq(found_user_id, true, "Should find user.id attribute")
  assert_eq(found_user_email, true, "Should find user.email attribute")
  assert_eq(found_ip_address, true, "Should find ip.address attribute")
}

test "log_severity_levels_and_convenience_methods" {
  // 测试日志严重性级别和便捷方法
  
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("test-logger", Some("1.0.0"), Some("https://example.com/schema"))
  
  // 测试所有严重性级别
  let trace_attributes = [
    ("operation.type", common::AttributeValue::string("debug")),
    ("trace.id", common::AttributeValue::string("trace-123"))
  ]
  
  let debug_attributes = [
    ("operation.type", common::AttributeValue::string("debug")),
    ("component", common::AttributeValue::string("database"))
  ]
  
  let info_attributes = [
    ("operation.type", common::AttributeValue::string("info")),
    ("event.type", common::AttributeValue::string("user_action"))
  ]
  
  let warn_attributes = [
    ("operation.type", common::AttributeValue::string("warning")),
    ("warning.type", common::AttributeValue::string("deprecated_api"))
  ]
  
  let error_attributes = [
    ("operation.type", common::AttributeValue::string("error")),
    ("error.code", common::AttributeValue::int(500L))
  ]
  
  let fatal_attributes = [
    ("operation.type", common::AttributeValue::string("fatal")),
    ("error.type", common::AttributeValue::string("system_crash"))
  ]
  
  // 测试便捷方法（这些操作不应该抛出异常）
  logger.debug("Debug message: entering function", Some(trace_attributes))
  logger.debug("Debug message: variable value", Some(debug_attributes))
  
  logger.info("Info message: user logged in", Some(info_attributes))
  logger.info("Info message: operation completed", None)
  
  logger.warn("Warning message: deprecated API used", Some(warn_attributes))
  logger.warn("Warning message: slow query detected", None)
  
  logger.error("Error message: database connection failed", Some(error_attributes))
  logger.error("Error message: validation failed", None)
  
  logger.fatal("Fatal message: system out of memory", Some(fatal_attributes))
  logger.fatal("Fatal message: critical service unavailable", None)
  
  // 测试空属性数组
  logger.debug("Debug with empty attributes", Some([]))
  logger.info("Info with empty attributes", Some([]))
  logger.warn("Warning with empty attributes", Some([]))
  logger.error("Error with empty attributes", Some([]))
  logger.fatal("Fatal with empty attributes", Some([]))
}

test "complex_log_record_with_trace_context" {
  // 测试带追踪上下文的复杂日志记录
  
  // 创建追踪ID和Span ID
  let trace_id = [0x0a, 0xf7, 0x65, 0x19, 0x16, 0xcd, 0x43, 0xdd, 0x84, 0x48, 0xeb, 0x21, 0x1c, 0x80, 0x31, 0x9c]
  let span_id = [0xb7, 0xad, 0x6b, 0x71, 0x69, 0x20, 0x33, 0x31]
  
  // 创建资源
  let resource = common::Resource::default("payment-service")
  
  // 创建仪表范围
  let instrumentation_scope = common::InstrumentationScope::{
    name: "payment-processor",
    version: Some("2.1.0"),
    schema_url: Some("https://opentelemetry.io/schema/1.0.0")
  }
  
  // 创建复杂日志记录
  let complex_log = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Error)
    .body("Payment processing failed")
    .with_attribute("error.code", common::AttributeValue::string("PAYMENT_DECLINED"))
    .with_attribute("error.message", common::AttributeValue::string("Insufficient funds"))
    .with_attribute("payment.amount", common::AttributeValue::float(99.99))
    .with_attribute("payment.currency", common::AttributeValue::string("USD"))
    .with_attribute("payment.method", common::AttributeValue::string("credit_card"))
    .with_attribute("user.id", common::AttributeValue::string("user-12345"))
    .with_attribute("merchant.id", common::AttributeValue::string("merchant-67890"))
    .with_attribute("transaction.id", common::AttributeValue::string("txn-abcdef123456"))
    .with_attribute("gateway.provider", common::AttributeValue::string("stripe"))
    .with_attribute("gateway.response_code", common::AttributeValue::int(4002L))
    .with_attribute("retry.count", common::AttributeValue::int(3L))
    .with_attribute("processing.duration_ms", common::AttributeValue::float(1250.5))
    .with_attribute("network.latency_ms", common::AttributeValue::float(45.2))
    .with_attribute("tags", common::AttributeValue::array_string(["payment", "error", "retry"]))
    .with_attribute("flags", common::AttributeValue::array_bool([true, false, true]))
    .build()
  
  // 设置追踪上下文和资源信息
  let log_with_context = logs::LogRecord::{
    ..complex_log,
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(0x01),
    resource: Some(resource),
    instrumentation_scope: Some(instrumentation_scope),
    observed_timestamp_unix_nanos: Some(1640995200000000500L)
  }
  
  // 验证追踪信息
  match log_with_context.trace_id {
    Some(trace) => {
      assert_eq(trace.length(), 16)
      assert_eq(trace[0], 0x0a)
      assert_eq(trace[15], 0x9c)
    }
    None => assert_eq(false, true, "Expected trace ID")
  }
  
  match log_with_context.span_id {
    Some(span) => {
      assert_eq(span.length(), 8)
      assert_eq(span[0], 0xb7)
      assert_eq(span[7], 0x31)
    }
    None => assert_eq(false, true, "Expected span ID")
  }
  
  match log_with_context.trace_flags {
    Some(flags) => assert_eq(flags, 0x01)
    None => assert_eq(false, true, "Expected trace flags")
  }
  
  // 验证资源信息
  match log_with_context.resource {
    Some(res) => {
      assert_eq(res.service_name, "payment-service")
      assert_eq(res.telemetry_sdk_name, "azimuth")
    }
    None => assert_eq(false, true, "Expected resource")
  }
  
  // 验证仪表范围
  match log_with_context.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "payment-processor")
      match scope.version {
        Some(v) => assert_eq(v, "2.1.0")
        None => assert_eq(false, true, "Expected scope version")
      }
    }
    None => assert_eq(false, true, "Expected instrumentation scope")
  }
  
  // 验证复杂属性
  assert_eq(log_with_context.attributes.length(), 16)
  
  // 验证数组属性
  let mut found_tags = false
  let mut found_flags = false
  
  let mut i = 0
  while i < log_with_context.attributes.length() {
    let (key, value) = log_with_context.attributes[i]
    
    if key == "tags" {
      match value {
        common::ArrayStringValue(tags) => {
          assert_eq(tags.length(), 3)
          assert_eq(tags.contains("payment"), true)
          assert_eq(tags.contains("error"), true)
          assert_eq(tags.contains("retry"), true)
          found_tags = true
        }
        _ => assert_eq(false, true, "Expected ArrayStringValue for tags")
      }
    }
    
    if key == "flags" {
      match value {
        common::ArrayBoolValue(flags) => {
          assert_eq(flags.length(), 3)
          assert_eq(flags[0], true)
          assert_eq(flags[1], false)
          assert_eq(flags[2], true)
          found_flags = true
        }
        _ => assert_eq(false, true, "Expected ArrayBoolValue for flags")
      }
    }
    
    i = i + 1
  }
  
  assert_eq(found_tags, true, "Should find tags attribute")
  assert_eq(found_flags, true, "Should find flags attribute")
}

test "log_record_validation_and_edge_cases" {
  // 测试日志记录验证和边界情况
  
  // 测试最小日志记录
  let minimal_log = logs::LogRecord::builder().build()
  
  assert_eq(minimal_log.timestamp_unix_nanos, 0L)
  match minimal_log.severity_number {
    logs::Info => assert_eq(true, true)  // 默认严重性
    _ => assert_eq(false, true, "Expected default Info severity")
  }
  assert_eq(minimal_log.severity_text, None)
  assert_eq(minimal_log.body, None)
  assert_eq(minimal_log.attributes.length(), 0)
  assert_eq(minimal_log.trace_id, None)
  assert_eq(minimal_log.span_id, None)
  assert_eq(minimal_log.trace_flags, None)
  assert_eq(minimal_log.resource, None)
  assert_eq(minimal_log.instrumentation_scope, None)
  
  // 测试空字符串和零值
  let empty_log = logs::LogRecord::builder()
    .timestamp(0L)
    .severity(logs::Trace)
    .body("")
    .with_attribute("empty.string", common::AttributeValue::string(""))
    .with_attribute("zero.int", common::AttributeValue::int(0L))
    .with_attribute("zero.float", common::AttributeValue::float(0.0))
    .with_attribute("false.bool", common::AttributeValue::bool(false))
    .with_attribute("empty.array", common::AttributeValue::array_string([]))
    .build()
  
  match empty_log.body {
    Some(body) => assert_eq(body.length(), 0)
    None => assert_eq(false, true, "Expected empty body")
  }
  
  // 测试极值
  let extreme_log = logs::LogRecord::builder()
    .timestamp(9223372036854775807L)  // 最大Int64值
    .severity(logs::Fatal)
    .body("Critical system error")
    .with_attribute("max.int", common::AttributeValue::int(9223372036854775807L))
    .with_attribute("min.int", common::AttributeValue::int(-9223372036854775808L))
    .with_attribute("max.float", common::AttributeValue::float(1.7976931348623157e+308))
    .with_attribute("min.float", common::AttributeValue::float(-1.7976931348623157e+308))
    .build()
  
  assert_eq(extreme_log.timestamp_unix_nanos, 9223372036854775807L)
  match extreme_log.severity_number {
    logs::Fatal => assert_eq(true, true)
    _ => assert_eq(false, true, "Expected Fatal severity")
  }
  
  // 测试长字符串
  let long_string = "This is a very long log message that tests the system's ability to handle extended messages without issues. ".repeat(10)
  let long_log = logs::LogRecord::builder()
    .body(long_string)
    .with_attribute("long.key.name.that.tests.the.systems.ability.to.handle.extended.key.names", common::AttributeValue::string("long value"))
    .build()
  
  match long_log.body {
    Some(body) => assert_eq(body.length() > 100, true)
    None => assert_eq(false, true, "Expected long body")
  }
  
  // 测试特殊字符和Unicode
  let unicode_log = logs::LogRecord::builder()
    .body("Unicode test: 测试 🚀 🌟 💫")
    .with_attribute("unicode.key.测试", common::AttributeValue::string("Unicode value: 测试 🚀"))
    .with_attribute("special.chars", common::AttributeValue::string("Special: @#$%^&*()_+-={}[]|\\:;\"'<>?,./"))
    .build()
  
  match unicode_log.body {
    Some(body) => {
      assert_eq(body.contains("测试"), true)
      assert_eq(body.contains("🚀"), true)
    }
    None => assert_eq(false, true, "Expected Unicode body")
  }
}

test "logger_provider_and_logger_hierarchy" {
  // 测试日志提供者和日志器层次结构
  
  // 创建多个日志器
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let http_logger = logger_provider.get_logger("http-logger", Some("1.0.0"), Some("https://example.com/http-schema"))
  let db_logger = logger_provider.get_logger("database-logger", Some("2.1.0"), None)
  let cache_logger = logger_provider.get_logger("cache-logger", None, Some("https://example.com/cache-schema"))
  
  // 测试不同日志器的日志记录
  let http_attributes = [
    ("logger.name", common::AttributeValue::string("http-logger")),
    ("logger.version", common::AttributeValue::string("1.0.0")),
    ("http.method", common::AttributeValue::string("GET")),
    ("http.status", common::AttributeValue::int(200L))
  ]
  
  let db_attributes = [
    ("logger.name", common::AttributeValue::string("database-logger")),
    ("logger.version", common::AttributeValue::string("2.1.0")),
    ("db.query", common::AttributeValue::string("SELECT * FROM users")),
    ("db.duration_ms", common::AttributeValue::float(25.5))
  ]
  
  let cache_attributes = [
    ("logger.name", common::AttributeValue::string("cache-logger")),
    ("cache.operation", common::AttributeValue::string("get")),
    ("cache.key", common::AttributeValue::string("user:12345")),
    ("cache.hit", common::AttributeValue::bool(true))
  ]
  
  // 使用不同日志器记录日志
  http_logger.info("HTTP request completed", Some(http_attributes))
  db_logger.debug("Database query executed", Some(db_attributes))
  cache_logger.info("Cache operation performed", Some(cache_attributes))
  
  // 测试相同名称的日志器创建
  let another_http_logger = logger_provider.get_logger("http-logger", Some("1.0.0"), Some("https://example.com/http-schema"))
  another_http_logger.warn("Another HTTP warning", None)
  
  // 测试所有严重性级别的便捷方法
  let test_logger = logger_provider.get_logger("test-logger", None, None)
  
  test_logger.debug("Debug message", None)
  test_logger.info("Info message", None)
  test_logger.warn("Warning message", None)
  test_logger.error("Error message", None)
  test_logger.fatal("Fatal message", None)
  
  // 测试带复杂属性的日志记录
  let complex_attributes = [
    ("string.value", common::AttributeValue::string("test")),
    ("int.value", common::AttributeValue::int(42L)),
    ("float.value", common::AttributeValue::float(3.14)),
    ("bool.value", common::AttributeValue::bool(true)),
    ("string.array", common::AttributeValue::array_string(["a", "b", "c"])),
    ("int.array", common::AttributeValue::array_int([1L, 2L, 3L])),
    ("float.array", common::AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool.array", common::AttributeValue::array_bool([true, false, true]))
  ]
  
  test_logger.info("Complex attribute test", Some(complex_attributes))
}

test "log_record_timestamp_and_observability" {
  // 测试日志记录时间戳和可观察性
  
  // 测试不同的时间戳格式
  let timestamp_log = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)  // 2022-01-01 00:00:00 UTC
    .severity(logs::Info)
    .body("Timestamp test")
    .build()
  
  assert_eq(timestamp_log.timestamp_unix_nanos, 1640995200000000000L)
  
  // 测试观察时间戳
  let observed_log = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("Observed timestamp test")
    .build()
  
  // 手动设置观察时间戳
  let log_with_observed = logs::LogRecord::{
    ..observed_log,
    observed_timestamp_unix_nanos: Some(1640995200000000500L)
  }
  
  match log_with_observed.observed_timestamp_unix_nanos {
    Some(observed) => {
      assert_eq(observed, 1640995200000000500L)
      assert_eq(observed > log_with_observed.timestamp_unix_nanos, true)
    }
    None => assert_eq(false, true, "Expected observed timestamp")
  }
  
  // 测试时间戳计算
  let start_timestamp = 1640995200000000000L
  let end_timestamp = 1640995200000001000L
  
  let duration_log = logs::LogRecord::builder()
    .timestamp(start_timestamp)
    .severity(logs::Info)
    .body("Duration test")
    .with_attribute("operation.start_time", common::AttributeValue::int(start_timestamp))
    .with_attribute("operation.end_time", common::AttributeValue::int(end_timestamp))
    .with_attribute("operation.duration_ns", common::AttributeValue::int(end_timestamp - start_timestamp))
    .with_attribute("operation.duration_ms", common::AttributeValue::float((end_timestamp - start_timestamp).to_double() / 1000000.0))
    .build()
  
  // 验证持续时间计算
  let mut found_duration = false
  let mut i = 0
  while i < duration_log.attributes.length() {
    let (key, value) = duration_log.attributes[i]
    
    if key == "operation.duration_ns" {
      match value {
        common::IntValue(duration) => {
          assert_eq(duration, 1000L)
          found_duration = true
        }
        _ => assert_eq(false, true, "Expected IntValue for duration")
      }
    }
    
    i = i + 1
  }
  
  assert_eq(found_duration, true, "Should find duration attribute")
  
  // 测试时间戳边界情况
  let min_timestamp = 0L
  let max_timestamp = 9223372036854775807L
  
  let min_log = logs::LogRecord::builder()
    .timestamp(min_timestamp)
    .severity(logs::Trace)
    .body("Minimum timestamp")
    .build()
  
  let max_log = logs::LogRecord::builder()
    .timestamp(max_timestamp)
    .severity(logs::Fatal)
    .body("Maximum timestamp")
    .build()
  
  assert_eq(min_log.timestamp_unix_nanos, min_timestamp)
  assert_eq(max_log.timestamp_unix_nanos, max_timestamp)
}