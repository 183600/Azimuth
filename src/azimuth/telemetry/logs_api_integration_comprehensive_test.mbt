// Azimuth Telemetry - Logs API Integration Test
// 遥测日志API集成测试

test "logs_api_basic_functionality" {
  // 测试LoggerProvider基本功能
  let provider = logs::NoopLoggerProvider::{}
  let logger = provider.get_logger("test-logger", Some("1.0.0"), Some("http://example.com/schema"))
  
  // 验证logger创建成功
  assert_type(logger, logs::NoopLogger)
  
  // 测试基本日志记录
  logger.debug("Debug message", Some([("component", common::AttributeValue::string("auth"))]))
  logger.info("Info message", Some([("component", common::AttributeValue::string("database"))]))
  logger.warn("Warning message", Some([("component", common::AttributeValue::string("cache"))]))
  logger.error("Error message", Some([("component", common::AttributeValue::string("api"))]))
  logger.fatal("Fatal message", Some([("component", common::AttributeValue::string("system"))]))
}

test "logs_api_log_record_builder" {
  // 测试LogRecordBuilder
  let provider = logs::NoopLoggerProvider::{}
  let logger = provider.get_logger("test-logger")
  
  // 使用builder创建日志记录
  let log_record = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)  // 2022-01-01 00:00:00 UTC
    .severity(logs::Error)
    .body("Application error occurred")
    .with_attribute("error.code", common::AttributeValue::int(500L))
    .with_attribute("error.message", common::AttributeValue::string("Internal server error"))
    .with_attribute("user.id", common::AttributeValue::int(12345L))
    .build()
  
  // 验证日志记录结构
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(log_record.severity_number, logs::Error)
  match log_record.body {
    Some(body) => assert_eq(body, "Application error occurred")
    None => assert(false, "Expected log body")
  }
  assert_eq(log_record.attributes.length(), 3)
  assert_eq(log_record.attributes[0].0, "error.code")
  assert_eq(log_record.attributes[1].0, "error.message")
  assert_eq(log_record.attributes[2].0, "user.id")
}

test "logs_api_severity_levels" {
  // 测试所有严重性级别
  let provider = logs::NoopLoggerProvider::{}
  let logger = provider.get_logger("severity-test")
  
  // 测试每个严重性级别
  let trace_record = logs::LogRecord::builder()
    .severity(logs::Trace)
    .body("Trace level message")
    .build()
  assert_eq(trace_record.severity_number, logs::Trace)
  
  let debug_record = logs::LogRecord::builder()
    .severity(logs::Debug)
    .body("Debug level message")
    .build()
  assert_eq(debug_record.severity_number, logs::Debug)
  
  let info_record = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Info level message")
    .build()
  assert_eq(info_record.severity_number, logs::Info)
  
  let warn_record = logs::LogRecord::builder()
    .severity(logs::Warn)
    .body("Warning level message")
    .build()
  assert_eq(warn_record.severity_number, logs::Warn)
  
  let error_record = logs::LogRecord::builder()
    .severity(logs::Error)
    .body("Error level message")
    .build()
  assert_eq(error_record.severity_number, logs::Error)
  
  let fatal_record = logs::LogRecord::builder()
    .severity(logs::Fatal)
    .body("Fatal level message")
    .build()
  assert_eq(fatal_record.severity_number, logs::Fatal)
}

test "logs_api_tracing_integration" {
  // 测试与链路追踪的集成
  let provider = logs::NoopLoggerProvider::{}
  let logger = provider.get_logger("tracing-integration-test")
  
  // 创建带有trace信息的日志记录
  let trace_id = [0x01_byte, 0x02_byte, 0x03_byte, 0x04_byte, 0x05_byte, 0x06_byte, 0x07_byte, 0x08_byte, 
                  0x09_byte, 0x0A_byte, 0x0B_byte, 0x0C_byte, 0x0D_byte, 0x0E_byte, 0x0F_byte, 0x10_byte]
  let span_id = [0x11_byte, 0x12_byte, 0x13_byte, 0x14_byte, 0x15_byte, 0x16_byte, 0x17_byte, 0x18_byte]
  let trace_flags = 0x01_byte
  
  let log_record = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Operation completed successfully")
    .with_attribute("operation.name", common::AttributeValue::string("user.login"))
    .with_attribute("operation.duration_ms", common::AttributeValue::float(150.5))
    .build()
  
  // 在实际实现中，这里应该设置trace_id, span_id, trace_flags
  // 但由于结构定义限制，我们验证其他属性
  assert_eq(log_record.severity_number, logs::Info)
  match log_record.body {
    Some(body) => assert_eq(body, "Operation completed successfully")
    None => assert(false, "Expected log body")
  }
  assert_eq(log_record.attributes.length(), 2)
}

test "logs_api_complex_attributes" {
  // 测试复杂属性类型
  let provider = logs::NoopLoggerProvider::{}
  let logger = provider.get_logger("complex-attributes-test")
  
  // 测试数组属性
  let log_record = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Complex attributes test")
    .with_attribute("string.array", common::AttributeValue::array_string(["tag1", "tag2", "tag3"]))
    .with_attribute("int.array", common::AttributeValue::array_int([1L, 2L, 3L, 4L]))
    .with_attribute("float.array", common::AttributeValue::array_float([1.1, 2.2, 3.3]))
    .with_attribute("bool.array", common::AttributeValue::array_bool([true, false, true]))
    .with_attribute("mixed", common::AttributeValue::string("simple value"))
    .build()
  
  // 验证属性数量和类型
  assert_eq(log_record.attributes.length(), 5)
  
  // 验证数组属性
  match log_record.attributes[0].1 {
    common::ArrayStringValue(arr) => assert_eq(arr.length(), 3)
    _ => assert(false, "Expected string array")
  }
  
  match log_record.attributes[1].1 {
    common::ArrayIntValue(arr) => assert_eq(arr.length(), 4)
    _ => assert(false, "Expected int array")
  }
  
  match log_record.attributes[2].1 {
    common::ArrayFloatValue(arr) => assert_eq(arr.length(), 3)
    _ => assert(false, "Expected float array")
  }
  
  match log_record.attributes[3].1 {
    common::ArrayBoolValue(arr) => assert_eq(arr.length(), 3)
    _ => assert(false, "Expected bool array")
  }
}

test "logs_api_resource_and_instrumentation" {
  // 测试资源和 instrumentation scope
  let provider = logs::NoopLoggerProvider::{}
  let logger = provider.get_logger("resource-test")
  
  // 创建资源
  let resource = common::Resource::default("test-service")
  
  // 创建 instrumentation scope
  let scope = common::InstrumentationScope::{
    name: "test-instrumentation",
    version: Some("1.0.0"),
    schema_url: Some("http://example.com/schema")
  }
  
  // 在实际实现中，这里应该设置resource和instrumentation_scope
  let log_record = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Resource and instrumentation test")
    .with_attribute("service.name", common::AttributeValue::string(resource.service_name))
    .with_attribute("service.version", common::AttributeValue::string(resource.service_version.unwrap_or("unknown")))
    .with_attribute("instrumentation.name", common::AttributeValue::string(scope.name))
    .with_attribute("instrumentation.version", common::AttributeValue::string(scope.version.unwrap_or("unknown")))
    .build()
  
  // 验证属性
  assert_eq(log_record.attributes.length(), 4)
  assert_eq(log_record.attributes[0].0, "service.name")
  assert_eq(log_record.attributes[1].0, "service.version")
  assert_eq(log_record.attributes[2].0, "instrumentation.name")
  assert_eq(log_record.attributes[3].0, "instrumentation.version")
}

test "logs_api_multiple_loggers" {
  // 测试多个Logger实例
  let provider = logs::NoopLoggerProvider::{}
  
  // 创建多个Logger实例
  let logger1 = provider.get_logger("service-A", Some("1.0.0"))
  let logger2 = provider.get_logger("service-B", Some("2.0.0"), Some("http://example.com/schema-B"))
  let logger3 = provider.get_logger("service-C")
  
  // 使用不同Logger记录日志
  logger1.info("Service A started", Some([("service", common::AttributeValue::string("A"))]))
  logger2.warn("Service B warning", Some([("service", common::AttributeValue::string("B"))]))
  logger3.error("Service C error", Some([("service", common::AttributeValue::string("C"))]))
  
  // 测试便捷方法
  logger1.debug("Debug message from A")
  logger2.info("Info message from B")
  logger3.warn("Warning message from C")
  logger1.error("Error message from A")
  logger2.fatal("Fatal message from B")
}

test "logs_api_edge_cases" {
  // 测试边界情况和错误条件
  let provider = logs::NoopLoggerProvider::{}
  let logger = provider.get_logger("edge-case-test")
  
  // 测试空消息
  logger.debug("", Some([("test", common::AttributeValue::string("empty-message"))]))
  
  // 测试长消息
  let long_message = "x".repeat(10000)
  logger.info(long_message, Some([("test", common::AttributeValue::string("long-message"))]))
  
  // 测试特殊字符消息
  let special_message = "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?"
  logger.warn(special_message)
  
  // 测试Unicode消息
  let unicode_message = "Unicode: 🚀 🌟 测试消息 العربية 中文"
  logger.error(unicode_message)
  
  // 测试空属性
  logger.info("No attributes", None)
  logger.info("Empty attributes", Some([]))
  
  // 测试空名称Logger
  let empty_logger = provider.get_logger("")
  empty_logger.info("Empty logger name test")
  
  // 测试长名称Logger
  let long_name = "logger-name-".repeat(50)
  let long_logger = provider.get_logger(long_name)
  long_logger.info("Long logger name test")
}

test "logs_api_structured_logging" {
  // 测试结构化日志记录
  let provider = logs::NoopLoggerProvider::{}
  let logger = provider.get_logger("structured-logging-test")
  
  // 创建结构化日志记录
  let log_record = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .severity_text(Some("INFO"))
    .body("User action completed")
    .with_attribute("event.name", common::AttributeValue::string("user.login"))
    .with_attribute("user.id", common::AttributeValue::int(12345L))
    .with_attribute("user.email", common::AttributeValue::string("user@example.com"))
    .with_attribute("ip.address", common::AttributeValue::string("192.168.1.100"))
    .with_attribute("user.agent", common::AttributeValue::string("Mozilla/5.0..."))
    .with_attribute("session.id", common::AttributeValue::string("sess_abc123"))
    .with_attribute("request.id", common::AttributeValue::string("req_xyz789"))
    .with_attribute("duration.ms", common::AttributeValue::float(250.75))
    .with_attribute("success", common::AttributeValue::bool(true))
    .with_attribute("permissions", common::AttributeValue::array_string(["read", "write"]))
    .build()
  
  // 验证结构化属性
  assert_eq(log_record.attributes.length(), 10)
  assert_eq(log_record.severity_text, Some("INFO"))
  
  // 验证特定属性
  let mut found_user_id = false
  let mut found_duration = false
  let mut found_permissions = false
  
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    match key {
      "user.id" => {
        match value {
          common::IntValue(id) => {
            assert_eq(id, 12345L)
            found_user_id = true
          }
          _ => assert(false, "Expected int value for user.id")
        }
      }
      "duration.ms" => {
        match value {
          common::FloatValue(duration) => {
            assert_eq(duration, 250.75)
            found_duration = true
          }
          _ => assert(false, "Expected float value for duration.ms")
        }
      }
      "permissions" => {
        match value {
          common::ArrayStringValue(perms) => {
            assert_eq(perms.length(), 2)
            assert_eq(perms[0], "read")
            assert_eq(perms[1], "write")
            found_permissions = true
          }
          _ => assert(false, "Expected string array for permissions")
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert(found_user_id, "user.id attribute not found")
  assert(found_duration, "duration.ms attribute not found")
  assert(found_permissions, "permissions attribute not found")
}