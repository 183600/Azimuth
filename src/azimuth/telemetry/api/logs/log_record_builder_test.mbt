// 日志记录构建器测试用例
test "log_record_builder_basic" {
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Error)
    .body("Test error message")
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
  assert_eq(log_record.attributes.length(), 0)
}

test "log_record_builder_with_attributes" {
  let log_record = LogRecord::builder()
    .timestamp(9876543210L)
    .severity(Info)
    .body("Info message")
    .with_attribute("user.id", AttributeValue::string("user123"))
    .with_attribute("request.id", AttributeValue::string("req456"))
    .with_attribute("response.time", AttributeValue::float(123.45))
    .build()
  
  assert_eq(log_record.timestamp_unix_nanos, 9876543210L)
  match log_record.severity_number {
    Info => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  match log_record.body {
    Some(body) => assert_eq(body, "Info message")
    None => @test.fail("Test failed")
  }
  assert_eq(log_record.attributes.length(), 3)
  
  // 验证第一个属性
  match log_record.attributes[0].1 {
    AttributeValue::StringValue(user_id) => assert_eq(user_id, "user123")
    _ => @test.fail("Test failed")
  }
  
  // 验证第二个属性
  match log_record.attributes[1].1 {
    AttributeValue::StringValue(req_id) => assert_eq(req_id, "req456")
    _ => @test.fail("Test failed")
  }
  
  // 验证第三个属性
  match log_record.attributes[2].1 {
    AttributeValue::FloatValue(response_time) => assert_eq(response_time, 123.45)
    _ => @test.fail("Test failed")
  }
}

test "log_record_builder_default_values" {
  let log_record = LogRecord::builder().build()
  
  assert_eq(log_record.timestamp_unix_nanos, 0L)
  match log_record.severity_number {
    Info => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  match log_record.body {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
  assert_eq(log_record.attributes.length(), 0)
  match log_record.observed_timestamp_unix_nanos {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
}

test "log_record_builder_complex_attributes" {
  let log_record = LogRecord::builder()
    .severity(Warn)
    .body("Warning with complex attributes")
    .with_attribute("tags", AttributeValue::array_string(["web", "api", "critical"]))
    .with_attribute("metrics", AttributeValue::array_float([1.1, 2.2, 3.3]))
    .with_attribute("status_codes", AttributeValue::array_int([200L, 404L, 500L]))
    .with_attribute("feature_flags", AttributeValue::array_bool([true, false, true]))
    .build()
  
  match log_record.severity_number {
    Warn => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  assert_eq(log_record.attributes.length(), 4)
  
  // 验证数组属性
  match log_record.attributes[0].1 {
    AttributeValue::ArrayStringValue(tags) => {
      assert_eq(tags.length(), 3)
      assert_eq(tags[0], "web")
      assert_eq(tags[1], "api")
      assert_eq(tags[2], "critical")
    }
    _ => @test.fail("Test failed")
  }
  
  match log_record.attributes[1].1 {
    AttributeValue::ArrayFloatValue(metrics) => {
      assert_eq(metrics.length(), 3)
      assert_eq(metrics[0], 1.1)
      assert_eq(metrics[1], 2.2)
      assert_eq(metrics[2], 3.3)
    }
    _ => @test.fail("Test failed")
  }
  
  match log_record.attributes[2].1 {
    AttributeValue::ArrayIntValue(status_codes) => {
      assert_eq(status_codes.length(), 3)
      assert_eq(status_codes[0], 200L)
      assert_eq(status_codes[1], 404L)
      assert_eq(status_codes[2], 500L)
    }
    _ => @test.fail("Test failed")
  }
  
  match log_record.attributes[3].1 {
    AttributeValue::ArrayBoolValue(feature_flags) => {
      assert_eq(feature_flags.length(), 3)
      assert_eq(feature_flags[0], true)
      assert_eq(feature_flags[1], false)
      assert_eq(feature_flags[2], true)
    }
    _ => @test.fail("Test failed")
  }
}

test "log_record_all_severity_levels" {
  let severity_levels = [Trace, Debug, Info, Warn, Error, Fatal]
  let mut index = 0
  
  while index < severity_levels.length() {
    let severity = severity_levels[index]
    let log_record = LogRecord::builder()
      .severity(severity)
      .body("Test message for severity level")
      .build()
    
    assert_eq(log_record.severity_number, severity)
    match log_record.body {
      Some(body) => assert_eq(body, "Test message for severity level")
      None => @test.fail("Test failed")
    }
    
    index = index + 1
  }
}