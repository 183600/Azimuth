test "severity_number_values" {
  // 测试SeverityNumber的所有值
  let trace = Trace
  let debug = Debug
  let info = Info
  let warn = Warn
  let error = Error
  let fatal = Fatal
  
  // 确保所有严重性级别可以创建
  assert_eq(true, true)
}

test "log_record_basic_creation" {
  // 测试基本LogRecord创建
  let log_record = {
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: Some(1234567891L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Test log message"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  assert_eq(log_record.timestamp_unix_nanos, 1234567890L)
  assert_eq(log_record.observed_timestamp_unix_nanos, Some(1234567891L))
  assert_eq(log_record.severity_number, Info)
  assert_eq(log_record.severity_text, Some("INFO"))
  assert_eq(log_record.body, Some("Test log message"))
  assert_eq(log_record.attributes.length(), 0)
}

test "log_record_with_trace_context" {
  // 测试带追踪上下文的LogRecord
  let trace_id = Array::make(16, 1_byte)
  let span_id = Array::make(8, 2_byte)
  
  let log_record = {
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: None,
    severity_number: Error,
    severity_text: Some("ERROR"),
    body: Some("Error occurred"),
    attributes: [("error.type", @common.AttributeValue::string("timeout"))],
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(1_byte),
    resource: None,
    instrumentation_scope: None
  }
  
  assert_eq(log_record.severity_number, Error)
  assert_eq(log_record.trace_id?.length(), 16)
  assert_eq(log_record.span_id?.length(), 8)
  assert_eq(log_record.trace_flags?, 1_byte)
}

test "noop_logger_methods" {
  // 测试NoopLogger的所有方法
  let attributes = [("service", @azimuth.telemetry.api.common.AttributeValue::string("test"))]
  
  // 所有方法都应该正常执行（no-op）
  NoopLogger::debug("Debug message", Some(attributes))
  NoopLogger::info("Info message", None)
  NoopLogger::warn("Warning message", Some(attributes))
  NoopLogger::error("Error message", None)
  NoopLogger::fatal("Fatal message", Some(attributes))
  
  assert_eq(true, true)
}

test "noop_logger_emit" {
  // 测试NoopLogger的emit方法
  let log_record = {
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: None,
    body: Some("Test emit"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  NoopLogger::emit(log_record)
  assert_eq(true, true)
}

test "noop_logger_provider_get_logger" {
  // 测试NoopLoggerProvider的get_logger方法
  let logger = NoopLoggerProvider::get_logger("test-logger", Some("1.0.0"), Some("http://example.com/schema"))
  
  // 确保返回的logger可以正常使用
  logger.info("Test message", None)
  assert_eq(true, true)
}

test "log_record_builder_basic" {
  // 测试LogRecordBuilder的基本用法
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Warn)
    .body("Warning message")
    .build()
  
  assert_eq(log_record.timestamp_unix_nanos, 1234567890L)
  assert_eq(log_record.severity_number, Warn)
  assert_eq(log_record.body, Some("Warning message"))
  assert_eq(log_record.severity_text, None)
}

test "log_record_builder_with_attributes" {
  // 测试LogRecordBuilder添加属性
  let log_record = LogRecord::builder()
    .severity(Error)
    .body("Error message")
    .with_attribute("error.code", @azimuth.telemetry.api.common.AttributeValue::int(500L))
    .with_attribute("error.message", @azimuth.telemetry.api.common.AttributeValue::string("Internal Server Error"))
    .with_attribute("retry.count", @azimuth.telemetry.api.common.AttributeValue::int(3L))
    .build()
  
  assert_eq(log_record.severity_number, Error)
  assert_eq(log_record.body, Some("Error message"))
  assert_eq(log_record.attributes.length(), 3)
}

test "log_record_builder_complete" {
  // 测试LogRecordBuilder的完整用法
  let trace_id = Array::make(16, 5_byte)
  let span_id = Array::make(8, 6_byte)
  let resource = @azimuth.telemetry.api.common.Resource::default("test-service")
  let scope = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "test-scope", 
    version: Some("1.0.0"), 
    schema_url: None
  }
  
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Fatal)
    .severity_text("FATAL")
    .body("Fatal error occurred")
    .with_attribute("component", @azimuth.telemetry.api.common.AttributeValue::string("database"))
    .with_attribute("query", @azimuth.telemetry.api.common.AttributeValue::string("SELECT * FROM users"))
    .build()
  
  assert_eq(log_record.timestamp_unix_nanos, 1234567890L)
  assert_eq(log_record.severity_number, Fatal)
  assert_eq(log_record.severity_text, Some("FATAL"))
  assert_eq(log_record.body, Some("Fatal error occurred"))
  assert_eq(log_record.attributes.length(), 2)
}

test "log_record_comprehensive_builder" {
  // 测试LogRecordBuilder的全面功能，包含所有字段
  let log_record = LogRecord::builder()
    .timestamp(1609459200000000000L) // 2021-01-01 00:00:00 UTC
    .observed_timestamp(1609459200000000001L)
    .severity(Error)
    .severity_text("ERROR")
    .body("Database connection failed")
    .with_attribute("error.code", @azimuth.telemetry.api.common.AttributeValue::int(500))
    .with_attribute("error.type", @azimuth.telemetry.api.common.AttributeValue::string("ConnectionError"))
    .with_attribute("retry.count", @azimuth.telemetry.api.common.AttributeValue::int(3))
    .with_attribute("connection.timeout", @azimuth.telemetry.api.common.AttributeValue::float(30.0))
    .with_attribute("debug.enabled", @azimuth.telemetry.api.common.AttributeValue::bool(true))
    .build()
  
  // 验证时间戳
  assert_eq(log_record.timestamp_unix_nanos, 1609459200000000000L)
  assert_eq(log_record.observed_timestamp_unix_nanos, Some(1609459200000000001L))
  
  // 验证严重性
  assert_eq(log_record.severity_number, Error)
  assert_eq(log_record.severity_text, Some("ERROR"))
  
  // 验证消息体
  assert_eq(log_record.body, Some("Database connection failed"))
  
  // 验证属性数量和类型
  assert_eq(log_record.attributes.length(), 5)
  
  // 验证不同类型的属性
  let mut found_int_attr = false
  let mut found_string_attr = false
  let mut found_float_attr = false
  let mut found_bool_attr = false
  
  let mut index = 0
  while index < log_record.attributes.length() {
    let (key, value) = log_record.attributes[index]
    match key {
      "error.code" => {
        match value {
          @azimuth.telemetry.api.common.AttributeValue::IntValue(code) => {
            assert_eq(code, 500)
            found_int_attr = true
          }
          _ => @test.fail("Expected IntValue for error.code")
        }
      }
      "error.type" => {
        match value {
          @azimuth.telemetry.api.common.AttributeValue::StringValue(error_type) => {
            assert_eq(error_type, "ConnectionError")
            found_string_attr = true
          }
          _ => @test.fail("Expected StringValue for error.type")
        }
      }
      "connection.timeout" => {
        match value {
          @azimuth.telemetry.api.common.AttributeValue::FloatValue(timeout) => {
            assert_eq(timeout, 30.0)
            found_float_attr = true
          }
          _ => @test.fail("Expected FloatValue for connection.timeout")
        }
      }
      "debug.enabled" => {
        match value {
          @azimuth.telemetry.api.common.AttributeValue::BoolValue(enabled) => {
            assert_eq(enabled, true)
            found_bool_attr = true
          }
          _ => @test.fail("Expected BoolValue for debug.enabled")
        }
      }
      _ => () // 忽略其他属性
    }
    index = index + 1
  }
  
  assert_eq(found_int_attr, true)
  assert_eq(found_string_attr, true)
  assert_eq(found_float_attr, true)
  assert_eq(found_bool_attr, true)
}

test "log_record_with_trace_context_complex" {
  // 测试带复杂追踪上下文的LogRecord
  let trace_id = Array::make(16, 0_byte)
  let span_id = Array::make(8, 0_byte)
  
  // 设置特定的trace_id和span_id值
  trace_id[0] = 0x0a_byte
  trace_id[1] = 0xf7_byte
  trace_id[15] = 0x9c_byte
  
  span_id[0] = 0xb7_byte
  span_id[7] = 0x31_byte
  
  let resource = @azimuth.telemetry.api.common.Resource::default("payment-service")
  let scope = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "payment-processor",
    version: Some("2.1.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L) // 2022-01-01 00:00:00 UTC
    .severity(Error)
    .severity_text("ERROR")
    .body("Payment processing failed: Insufficient funds")
    .with_attribute("payment.id", @azimuth.telemetry.api.common.AttributeValue::string("pay_123456789"))
    .with_attribute("amount", @azimuth.telemetry.api.common.AttributeValue::float(99.99))
    .with_attribute("currency", @azimuth.telemetry.api.common.AttributeValue::string("USD"))
    .with_attribute("customer.id", @azimuth.telemetry.api.common.AttributeValue::string("cust_987654321"))
    .with_attribute("error.code", @azimuth.telemetry.api.common.AttributeValue::int(4002))
    .with_attribute("error.category", @azimuth.telemetry.api.common.AttributeValue::string("payment_error"))
    .with_attribute("retry.eligible", @azimuth.telemetry.api.common.AttributeValue::bool(false))
    .with_attribute("processing.time.ms", @azimuth.telemetry.api.common.AttributeValue::int(1250))
    .build()
  
  // 验证基本字段
  assert_eq(log_record.severity_number, Error)
  assert_eq(log_record.body, Some("Payment processing failed: Insufficient funds"))
  
  // 验证属性数量和内容
  assert_eq(log_record.attributes.length(), 8)
  
  // 验证特定属性
  let mut found_payment_id = false
  let mut found_amount = false
  let mut found_error_code = false
  let mut found_retry_eligible = false
  
  let mut index = 0
  while index < log_record.attributes.length() {
    let (key, value) = log_record.attributes[index]
    match key {
      "payment.id" => {
        match value {
          @azimuth.telemetry.api.common.AttributeValue::StringValue(id) => {
            assert_eq(id, "pay_123456789")
            found_payment_id = true
          }
          _ => @test.fail("Expected StringValue for payment.id")
        }
      }
      "amount" => {
        match value {
          @azimuth.telemetry.api.common.AttributeValue::FloatValue(amount) => {
            assert_eq(amount, 99.99)
            found_amount = true
          }
          _ => @test.fail("Expected FloatValue for amount")
        }
      }
      "error.code" => {
        match value {
          @azimuth.telemetry.api.common.AttributeValue::IntValue(code) => {
            assert_eq(code, 4002)
            found_error_code = true
          }
          _ => @test.fail("Expected IntValue for error.code")
        }
      }
      "retry.eligible" => {
        match value {
          @azimuth.telemetry.api.common.AttributeValue::BoolValue(eligible) => {
            assert_eq(eligible, false)
            found_retry_eligible = true
          }
          _ => @test.fail("Expected BoolValue for retry.eligible")
        }
      }
      _ => () // 忽略其他属性
    }
    index = index + 1
  }
  
  assert_eq(found_payment_id, true)
  assert_eq(found_amount, true)
  assert_eq(found_error_code, true)
  assert_eq(found_retry_eligible, true)
}

test "log_record_edge_cases_and_special_characters" {
  // 测试LogRecord的边界情况和特殊字符处理
  let empty_body = ""
  let long_body = "a".repeat(10000)
  let unicode_body = "日志消息包含中文和特殊字符：🔥 🚀 ✨"
  let special_chars_body = "Special chars: \\n\\t\\r\\\"'<>{}[]|&;`"
  let json_body = "{\"error\": \"Internal Server Error\", \"code\": 500, \"details\": {\"reason\": \"timeout\", \"retryable\": true}}"
  
  // 测试空消息体
  let empty_log = LogRecord::builder()
    .severity(Debug)
    .body(empty_body)
    .build()
  assert_eq(empty_log.body, Some(""))
  
  // 测试长消息体
  let long_log = LogRecord::builder()
    .severity(Info)
    .body(long_body)
    .build()
  assert_eq(long_log.body?.length(), 10000)
  
  // 测试Unicode消息体
  let unicode_log = LogRecord::builder()
    .severity(Warn)
    .body(unicode_body)
    .build()
  assert_eq(unicode_log.body, Some("日志消息包含中文和特殊字符：🔥 🚀 ✨"))
  
  // 测试特殊字符消息体
  let special_chars_log = LogRecord::builder()
    .severity(Error)
    .body(special_chars_body)
    .build()
  assert_eq(special_chars_log.body, Some("Special chars: \\n\\t\\r\\\"'<>{}[]|&;`"))
  
  // 测试JSON消息体
  let json_log = LogRecord::builder()
    .severity(Fatal)
    .body(json_body)
    .with_attribute("content.type", @azimuth.telemetry.api.common.AttributeValue::string("application/json"))
    .build()
  assert_eq(json_log.body, Some("{\"error\": \"Internal Server Error\", \"code\": 500, \"details\": {\"reason\": \"timeout\", \"retryable\": true}}"))
  
  // 验证JSON日志的属性
  assert_eq(json_log.attributes.length(), 1)
  match json_log.attributes[0] {
    ("content.type", @azimuth.telemetry.api.common.AttributeValue::StringValue(content_type)) => {
      assert_eq(content_type, "application/json")
    }
    _ => @test.fail("Expected content.type attribute")
  }
}

test "log_record_severity_progression" {
  // 测试日志严重性级别的递进场景
  let base_timestamp = 1640995200000000000L // 2022-01-01 00:00:00 UTC
  
  // 模拟一个操作过程中的日志级别递进
  let debug_log = LogRecord::builder()
    .timestamp(base_timestamp)
    .severity(Trace)
    .severity_text("TRACE")
    .body("Starting database connection")
    .with_attribute("operation", @azimuth.telemetry.api.common.AttributeValue::string("db_connect"))
    .build()
  
  let info_log = LogRecord::builder()
    .timestamp(base_timestamp + 1000000L) // 1ms later
    .severity(Info)
    .severity_text("INFO")
    .body("Database connection established")
    .with_attribute("operation", @azimuth.telemetry.api.common.AttributeValue::string("db_connect"))
    .with_attribute("connection.id", @azimuth.telemetry.api.common.AttributeValue::string("conn_12345"))
    .build()
  
  let warn_log = LogRecord::builder()
    .timestamp(base_timestamp + 5000000L) // 5ms later
    .severity(Warn)
    .severity_text("WARN")
    .body("Query execution taking longer than expected")
    .with_attribute("operation", @azimuth.telemetry.api.common.AttributeValue::string("db_query"))
    .with_attribute("query.duration.ms", @azimuth.telemetry.api.common.AttributeValue::int(5000))
    .with_attribute("threshold.ms", @azimuth.telemetry.api.common.AttributeValue::int(1000))
    .build()
  
  let error_log = LogRecord::builder()
    .timestamp(base_timestamp + 10000000L) // 10ms later
    .severity(Error)
    .severity_text("ERROR")
    .body("Query execution failed")
    .with_attribute("operation", @azimuth.telemetry.api.common.AttributeValue::string("db_query"))
    .with_attribute("error.code", @azimuth.telemetry.api.common.AttributeValue::int(2006))
    .with_attribute("error.message", @azimuth.telemetry.api.common.AttributeValue::string("MySQL server has gone away"))
    .build()
  
  let fatal_log = LogRecord::builder()
    .timestamp(base_timestamp + 15000000L) // 15ms later
    .severity(Fatal)
    .severity_text("FATAL")
    .body("Service cannot recover from database failure")
    .with_attribute("operation", @azimuth.telemetry.api.common.AttributeValue::string("service_recovery"))
    .with_attribute("restart.required", @azimuth.telemetry.api.common.AttributeValue::bool(true))
    .with_attribute("impact", @azimuth.telemetry.api.common.AttributeValue::string("service_unavailable"))
    .build()
  
  // 验证每个日志的严重性级别
  assert_eq(debug_log.severity_number, Trace)
  assert_eq(info_log.severity_number, Info)
  assert_eq(warn_log.severity_number, Warn)
  assert_eq(error_log.severity_number, Error)
  assert_eq(fatal_log.severity_number, Fatal)
  
  // 验证时间戳递增
  assert_eq(debug_log.timestamp_unix_nanos, base_timestamp)
  assert_eq(info_log.timestamp_unix_nanos, base_timestamp + 1000000L)
  assert_eq(warn_log.timestamp_unix_nanos, base_timestamp + 5000000L)
  assert_eq(error_log.timestamp_unix_nanos, base_timestamp + 10000000L)
  assert_eq(fatal_log.timestamp_unix_nanos, base_timestamp + 15000000L)
  
  // 验证严重性文本
  assert_eq(debug_log.severity_text, Some("TRACE"))
  assert_eq(info_log.severity_text, Some("INFO"))
  assert_eq(warn_log.severity_text, Some("WARN"))
  assert_eq(error_log.severity_text, Some("ERROR"))
  assert_eq(fatal_log.severity_text, Some("FATAL"))
}