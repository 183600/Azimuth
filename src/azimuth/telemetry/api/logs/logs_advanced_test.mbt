// Logs模块高级测试用例

test "log_record_complex_scenarios" {
  // 测试复杂场景的LogRecord
  
  // 创建带有trace信息的复杂日志记录
  let trace_id = Array::make(16, 0x01_byte)
  let span_id = Array::make(8, 0x02_byte)
  
  let complex_log = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Error)
    .body("Database connection failed")
    .with_attribute("service.name", AttributeValue::string("payment-service"))
    .with_attribute("service.version", AttributeValue::string("2.1.0"))
    .with_attribute("error.code", AttributeValue::string("DB_CONN_FAILED"))
    .with_attribute("error.retries", AttributeValue::int(3L))
    .with_attribute("error.timeout_ms", AttributeValue::float(30000.0))
    .with_attribute("error.recoverable", AttributeValue::bool(false))
    .with_attribute("affected.endpoints", AttributeValue::array_string(["/api/pay", "/api/verify"]))
    .with_attribute("error.codes", AttributeValue::array_int([1001L, 1002L, 1003L]))
    .with_attribute("retry.delays", AttributeValue::array_float([1000.0, 2000.0, 4000.0]))
    .with_attribute("debug.flags", AttributeValue::array_bool([true, false, true]))
    .build()
  
  // 手动设置trace相关字段（因为builder中没有这些方法）
  let log_with_trace = LogRecord::{
    timestamp_unix_nanos: complex_log.timestamp_unix_nanos,
    observed_timestamp_unix_nanos: complex_log.observed_timestamp_unix_nanos,
    severity_number: complex_log.severity_number,
    severity_text: complex_log.severity_text,
    body: complex_log.body,
    attributes: complex_log.attributes,
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(1_byte),
    resource: Some(Resource::default("payment-service")),
    instrumentation_scope: None
  }
  
  // 验证基本属性
  assert_eq(log_with_trace.timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(log_with_trace.severity_number, Error)
  
  match log_with_trace.body {
    Some(body) => assert_eq(body, "Database connection failed")
    None => @test.fail("Expected Some(body)")
  }
  
  // 验证trace信息
  match log_with_trace.trace_id {
    Some(trace) => {
      assert_eq(trace.length(), 16)
      assert_eq(trace[0], 0x01_byte)
    }
    None => @test.fail("Expected Some(trace_id)")
  }
  
  match log_with_trace.span_id {
    Some(span) => {
      assert_eq(span.length(), 8)
      assert_eq(span[0], 0x02_byte)
    }
    None => @test.fail("Expected Some(span_id)")
  }
  
  match log_with_trace.trace_flags {
    Some(flags) => assert_eq(flags, 1_byte)
    None => @test.fail("Expected Some(trace_flags)")
  }
  
  // 验证复杂属性
  assert_eq(log_with_trace.attributes.length(), 10)
  
  // 验证字符串属性
  match log_with_trace.attributes[0].1 {
    StringValue(name) => assert_eq(name, "payment-service")
    _ => @test.fail("Expected StringValue")
  }
  
  // 验证整数属性
  match log_with_trace.attributes[2].1 {
    IntValue(code) => assert_eq(code, 3L)
    _ => @test.fail("Expected IntValue")
  }
  
  // 验证浮点数属性
  match log_with_trace.attributes[3].1 {
    FloatValue(timeout) => assert_eq(timeout, 30000.0)
    _ => @test.fail("Expected FloatValue")
  }
  
  // 验证布尔属性
  match log_with_trace.attributes[4].1 {
    BoolValue(recoverable) => assert_eq(recoverable, false)
    _ => @test.fail("Expected BoolValue")
  }
  
  // 验证字符串数组属性
  match log_with_trace.attributes[5].1 {
    ArrayStringValue(endpoints) => {
      assert_eq(endpoints.length(), 2)
      assert_eq(endpoints[0], "/api/pay")
      assert_eq(endpoints[1], "/api/verify")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
}

test "logger_convenience_methods" {
  // 测试Logger的便捷方法
  
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("test-logger", Some("1.0.0"), Some("https://example.com/schema"))
  
  // 测试debug方法
  logger.debug("Debug message", [
    ("debug.level", AttributeValue::int(1L)),
    ("debug.component", AttributeValue::string("auth"))
  ])
  
  // 测试info方法
  logger.info("Info message", [
    ("info.level", AttributeValue::int(2L)),
    ("info.component", AttributeValue::string("database")),
    ("info.operation", AttributeValue::string("query"))
  ])
  
  // 测试warn方法
  logger.warn("Warning message", [
    ("warn.level", AttributeValue::int(3L)),
    ("warn.component", AttributeValue::string("cache")),
    ("warn.expiry", AttributeValue::float(3600.0))
  ])
  
  // 测试error方法
  logger.error("Error message", [
    ("error.level", AttributeValue::int(4L)),
    ("error.component", AttributeValue::string("payment")),
    ("error.code", AttributeValue::string("PAYMENT_FAILED")),
    ("error.recoverable", AttributeValue::bool(true))
  ])
  
  // 测试fatal方法
  logger.fatal("Fatal message", [
    ("fatal.level", AttributeValue::int(5L)),
    ("fatal.component", AttributeValue::string("system")),
    ("fatal.shutdown", AttributeValue::bool(true)),
    ("fatal.grace_period", AttributeValue::int(30L))
  ])
  
  // 测试带有复杂属性的便捷方法
  let complex_attrs : Attributes = [
    ("service.namespace", AttributeValue::string("production")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("deployment.environment", AttributeValue::string("prod")),
    ("host.name", AttributeValue::string("web-server-01")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.cpu.percent", AttributeValue::float(75.5)),
    ("process.healthy", AttributeValue::bool(true)),
    ("tags", AttributeValue::array_string(["web", "api", "critical"])),
    ("ports", AttributeValue::array_int([8080L, 8443L, 9090L])),
    ("cpu.thresholds", AttributeValue::array_float([60.0, 80.0, 95.0])),
    ("feature.flags", AttributeValue::array_bool([true, false, true]))
  ]
  
  logger.info("Complex log with many attributes", complex_attrs)
  
  // 测试空属性
  logger.info("Simple log message", [])
  
  // 测试None属性
  logger.info("Another simple log", None)
  
  // 测试特殊字符的消息
  logger.debug("Debug with special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?", [])
  logger.info("Unicode测试消息🚀🌟💫", [])
  logger.warn("Multi-line\nwarning\nmessage", [])
  logger.error("Message with \"quotes\" and 'apostrophes'", [])
  
  // 测试边界值
  logger.debug("Empty string test: \"\"", [])
  logger.info("Very long message: " + "A".repeat(1000), [])
  logger.warn("Number test: 0, -1, 9223372036854775807", [])
}