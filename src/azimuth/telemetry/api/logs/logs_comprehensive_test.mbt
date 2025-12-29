// Logs模块综合测试用例
test "log_record_builder_basic" {
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Info)
    .body("Test log message")
    .build()
  
  assert_eq(log_record.timestamp_unix_nanos, 1234567890L)
  match log_record.severity_number {
    Info => assert_eq(true, true)
    _ => @test.fail("Expected Info severity")
  }
  match log_record.body {
    Some(body) => assert_eq(body, "Test log message")
    None => @test.fail("Expected Some(body)")
  }
  assert_eq(log_record.attributes.length(), 0)
}

test "log_record_with_attributes" {
  let log_record = LogRecord::builder()
    .severity(Error)
    .body("Error occurred")
    .with_attribute("error.code", AttributeValue::int(500L))
    .with_attribute("error.message", AttributeValue::string("Internal Server Error"))
    .with_attribute("retry.count", AttributeValue::int(3L))
    .build()
  
  assert_eq(log_record.attributes.length(), 3)
  
  // 验证第一个属性
  match log_record.attributes[0].1 {
    IntValue(code) => assert_eq(code, 500L)
    _ => @test.fail("Expected IntValue for error.code")
  }
  
  // 验证第二个属性
  match log_record.attributes[1].1 {
    StringValue(message) => assert_eq(message, "Internal Server Error")
    _ => @test.fail("Expected StringValue for error.message")
  }
  
  // 验证第三个属性
  match log_record.attributes[2].1 {
    IntValue(count) => assert_eq(count, 3L)
    _ => @test.fail("Expected IntValue for retry.count")
  }
}
