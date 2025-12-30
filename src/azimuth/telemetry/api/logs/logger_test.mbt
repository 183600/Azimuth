// Logs模块日志记录器测试
// 测试日志记录器的创建和日志记录功能

test "logger_creation_and_basic_usage" {
  let provider = NoopLoggerProvider::{}
  let logger = provider.get_logger("test_logger", Some("1.0.0"))
  
  // 测试基本日志方法
  logger.debug("Debug message")
  logger.info("Info message")
  logger.warn("Warning message")
  logger.error("Error message")
  logger.fatal("Fatal message")
  
  @test.succeed()  // Noop实现，如果没有异常就算成功
}

test "logger_with_attributes" {
  let provider = NoopLoggerProvider::{}
  let logger = provider.get_logger("attribute_logger")
  
  let attributes = [
    ("user_id", @azimuth.telemetry.api.common.AttributeValue::string("12345")),
    ("request_id", @azimuth.telemetry.api.common.AttributeValue::string("req-67890")),
    ("method", @azimuth.telemetry.api.common.AttributeValue::string("POST"))
  ]
  
  logger.info("User action completed", Some(attributes))
  logger.error("Database connection failed", Some([
    ("error_code", @azimuth.telemetry.api.common.AttributeValue::int(500L)),
    ("retry_count", @azimuth.telemetry.api.common.AttributeValue::int(3L)),
    ("timeout", @azimuth.telemetry.api.common.AttributeValue::float(30.0))
  ]))
  
  @test.succeed()
}

test "log_record_builder_basic" {
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Info)
    .body("Application started successfully")
    .build()
  
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(log_record.observed_timestamp_unix_nanos, None)
  match log_record.severity_number {
    Info => @test.succeed()
    _ => @test.fail("Expected Info severity")
  }
  assert_eq(log_record.severity_text, None)
  match log_record.body {
    Some(message) => assert_eq(message, "Application started successfully")
    None => @test.fail("Expected body message")
  }
  assert_eq(log_record.attributes.length(), 0)
}

test "log_record_builder_with_attributes" {
  let log_record = LogRecord::builder()
    .severity(Error)
    .body("Request processing failed")
    .with_attribute("error_code", @azimuth.telemetry.api.common.AttributeValue::int(400L))
    .with_attribute("client_ip", @azimuth.telemetry.api.common.AttributeValue::string("192.168.1.100"))
    .with_attribute("retry_attempt", @azimuth.telemetry.api.common.AttributeValue::bool(true))
    .build()
  
  assert_eq(log_record.attributes.length(), 3)
  
  match log_record.attributes[0] {
    (key, @azimuth.telemetry.api.common.IntValue(value)) => {
      assert_eq(key, "error_code")
      assert_eq(value, 400L)
    }
    _ => @test.fail("Expected int attribute")
  }
  
  match log_record.attributes[1] {
    (key, @azimuth.telemetry.api.common.StringValue(value)) => {
      assert_eq(key, "client_ip")
      assert_eq(value, "192.168.1.100")
    }
    _ => @test.fail("Expected string attribute")
  }
  
  match log_record.attributes[2] {
    (key, @azimuth.telemetry.api.common.BoolValue(value)) => {
      assert_eq(key, "retry_attempt")
      assert_eq(value, true)
    }
    _ => @test.fail("Expected bool attribute")
  }
}

test "log_record_severity_levels" {
  let severity_levels = [
    Trace, Debug, Info, Warn, Error, Fatal
  ]
  
  let mut index = 0
  while index < severity_levels.length() {
    let severity = severity_levels[index]
    let log_record = LogRecord::builder()
      .severity(severity)
      .body("Test message")
      .build()
    
    match log_record.severity_number {
      Trace if index == 0 => @test.succeed()
      Debug if index == 1 => @test.succeed()
      Info if index == 2 => @test.succeed()
      Warn if index == 3 => @test.succeed()
      Error if index == 4 => @test.succeed()
      Fatal if index == 5 => @test.succeed()
      _ => @test.fail("Severity level mismatch")
    }
    index = index + 1
  }
}

test "log_record_with_trace_context" {
  let trace_id = [0_byte, 1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 
                  8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte]
  let span_id = [0_byte, 1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte]
  let trace_flags = 1_byte
  
  let log_record = LogRecord::builder()
    .severity(Warn)
    .body("Operation with trace context")
    .build()
  
  // 在实际实现中，这些字段应该通过builder设置
  // 这里我们验证结构体字段的存在
  assert_eq(log_record.trace_id, None)  // Noop实现中为None
  assert_eq(log_record.span_id, None)
  assert_eq(log_record.trace_flags, None)
}

test "log_record_comprehensive" {
  let resource = @azimuth.telemetry.api.common.Resource::default("test_service")
  let instrumentation_scope = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "test_scope",
    version: Some("1.0.0"),
    schema_url: Some("http://example.com/schema")
  }
  
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000001000L),
    severity_number: Error,
    severity_text: Some("ERROR"),
    body: Some("Critical system error"),
    attributes: [
      ("error_type", @azimuth.telemetry.api.common.AttributeValue::string("NullPointer")),
      ("stack_trace", @azimuth.telemetry.api.common.AttributeValue::string("at com.example.Main.main(Main.java:10)"))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(resource),
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  match log_record.observed_timestamp_unix_nanos {
    Some(timestamp) => assert_eq(timestamp, 1640995200000001000L)
    None => @test.fail("Expected observed timestamp")
  }
  match log_record.severity_text {
    Some(text) => assert_eq(text, "ERROR")
    None => @test.fail("Expected severity text")
  }
  match log_record.body {
    Some(message) => assert_eq(message, "Critical system error")
    None => @test.fail("Expected body message")
  }
  assert_eq(log_record.attributes.length(), 2)
  match log_record.resource {
    Some(res) => assert_eq(res.service_name, "test_service")
    None => @test.fail("Expected resource")
  }
  match log_record.instrumentation_scope {
    Some(scope) => assert_eq(scope.name, "test_scope")
    None => @test.fail("Expected instrumentation scope")
  }
}

test "logger_provider_multiple_loggers" {
  let provider = NoopLoggerProvider::{}
  
  let logger1 = provider.get_logger("auth-service", Some("1.2.3"))
  let logger2 = provider.get_logger("payment-service", Some("2.0.0"), Some("http://example.com/payment-schema"))
  let logger3 = provider.get_logger("notification-service")
  
  // 测试不同日志器的使用
  logger1.info("User authenticated successfully")
  logger2.error("Payment processing failed")
  logger3.warn("Notification queue is full")
  
  @test.succeed()
}