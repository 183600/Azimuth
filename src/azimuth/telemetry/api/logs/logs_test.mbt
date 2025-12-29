test "log_record_comprehensive" {
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Error)
    .body("Test error message")
    .with_attribute("error.code", AttributeValue::int(500L))
    .with_attribute("error.message", AttributeValue::string("Internal Server Error"))
    .with_attribute("service.name", AttributeValue::string("api-service"))
    .build()
  
  assert_eq(log_record.timestamp_unix_nanos, 1234567890L)
  match log_record.severity_number {
    Error => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  match log_record.body {
    Some(body) => assert_eq(body, "Test error message")
    None => @test.fail("Test failed")
  }
  assert_eq(log_record.attributes.length(), 3)
  
  // 验证属性值
  match log_record.attributes[0].1 {
    AttributeValue::IntValue(code) => assert_eq(code, 500L)
    _ => @test.fail("Test failed")
  }
}

test "log_record_various_severity" {
  // 测试不同严重性的日志记录
  let info_log = LogRecord::builder()
    .severity(Info)
    .body("Info message")
    .build()
  
  let warn_log = LogRecord::builder()
    .severity(Warn)
    .body("Warning message")
    .build()
  
  let fatal_log = LogRecord::builder()
    .severity(Fatal)
    .body("Fatal error message")
    .build()
  
  match info_log.severity_number {
    Info => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match warn_log.severity_number {
    Warn => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match fatal_log.severity_number {
    Fatal => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  // 验证body内容
  match info_log.body {
    Some(body) => assert_eq(body, "Info message")
    None => @test.fail("Test failed")
  }
}