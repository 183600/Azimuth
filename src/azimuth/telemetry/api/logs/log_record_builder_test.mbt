test "log_record_builder_comprehensive" {
  // 测试LogRecordBuilder的各种方法组合
  
  // 测试基本的builder模式
  let builder1 = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Error)
    .body("Test message")
  
  let log1 = builder1.build()
  assert_eq(log1.timestamp_unix_nanos, 1234567890L)
  match log1.severity_number {
    Error => assert_eq(true, true)
    _ => @test.fail("Expected Error severity")
  }
  match log1.body {
    Some(body) => assert_eq(body, "Test message")
    None => @test.fail("Expected Some(body)")
  }
  
  // 测试多个属性的添加
  let builder2 = LogRecord::builder()
    .timestamp(9876543210L)
    .severity(Info)
    .body("Info message")
    .with_attribute("key1", AttributeValue::string("value1"))
    .with_attribute("key2", AttributeValue::int(42L))
    .with_attribute("key3", AttributeValue::float(3.14))
    .with_attribute("key4", AttributeValue::bool(true))
  
  let log2 = builder2.build()
  assert_eq(log2.attributes.length(), 4)
  
  match log2.attributes[0].1 {
    AttributeValue::StringValue(s) => assert_eq(s, "value1")
    _ => @test.fail("Expected StringValue")
  }
  
  match log2.attributes[1].1 {
    AttributeValue::IntValue(i) => assert_eq(i, 42L)
    _ => @test.fail("Expected IntValue")
  }
  
  match log2.attributes[2].1 {
    AttributeValue::FloatValue(f) => assert_eq(f, 3.14)
    _ => @test.fail("Expected FloatValue")
  }
  
  match log2.attributes[3].1 {
    AttributeValue::BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  // 测试空builder
  let empty_builder = LogRecord::builder()
  let empty_log = empty_builder.build()
  
  assert_eq(empty_log.timestamp_unix_nanos, 0L) // 默认值
  match empty_log.severity_number {
    Info => assert_eq(true, true) // 默认值
    _ => @test.fail("Expected Info severity")
  }
  match empty_log.body {
    Some(_) => @test.fail("Expected None for body")
    None => assert_eq(true, true)
  }
  assert_eq(empty_log.attributes.length(), 0)
  
  // 测试只有部分字段的builder
  let partial_builder = LogRecord::builder()
    .severity(Warn)
  
  let partial_log = partial_builder.build()
  assert_eq(partial_log.timestamp_unix_nanos, 0L) // 默认值
  match partial_log.severity_number {
    Warn => assert_eq(true, true)
    _ => @test.fail("Expected Warn severity")
  }
  match partial_log.body {
    Some(_) => @test.fail("Expected None for body")
    None => assert_eq(true, true)
  }
  
  // 测试复杂属性
  let complex_builder = LogRecord::builder()
    .timestamp(1111111111L)
    .severity(Fatal)
    .body("Fatal error occurred")
    .with_attribute("service.name", AttributeValue::string("api-service"))
    .with_attribute("service.version", AttributeValue::string("2.1.0"))
    .with_attribute("error.code", AttributeValue::int(500L))
    .with_attribute("error.message", AttributeValue::string("Internal Server Error"))
    .with_attribute("request.id", AttributeValue::string("req-123456"))
    .with_attribute("user.id", AttributeValue::int(789L))
    .with_attribute("response.time", AttributeValue::float(1234.56))
    .with_attribute("server.healthy", AttributeValue::bool(false))
    .with_attribute("tags", AttributeValue::array_string(["critical", "api", "error"]))
    .with_attribute("ports", AttributeValue::array_int([8080L, 8443L]))
    .with_attribute("thresholds", AttributeValue::array_float([0.5, 0.9, 0.99]))
    .with_attribute("features", AttributeValue::array_bool([true, false, true]))
  
  let complex_log = complex_builder.build()
  assert_eq(complex_log.attributes.length(), 12)
  
  // 验证数组属性
  match complex_log.attributes[9].1 {
    AttributeValue::ArrayStringValue(tags) => {
      assert_eq(tags.length(), 3)
      assert_eq(tags[0], "critical")
      assert_eq(tags[1], "api")
      assert_eq(tags[2], "error")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match complex_log.attributes[10].1 {
    AttributeValue::ArrayIntValue(ports) => {
      assert_eq(ports.length(), 2)
      assert_eq(ports[0], 8080L)
      assert_eq(ports[1], 8443L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  // 测试特殊字符和Unicode
  let special_builder = LogRecord::builder()
    .body("Special message: !@#$%^&*()")
    .with_attribute("special.key", AttributeValue::string("special!@#$%^&*()"))
  
  let special_log = special_builder.build()
  match special_log.body {
    Some(body) => assert_eq(body, "Special message: !@#$%^&*()")
    None => @test.fail("Expected Some(body)")
  }
  
  let unicode_builder = LogRecord::builder()
    .body("Unicode消息: 测试🚀")
    .with_attribute("unicode.key", AttributeValue::string("Unicode值: 测试🎉"))
  
  let unicode_log = unicode_builder.build()
  match unicode_log.body {
    Some(body) => assert_eq(body, "Unicode消息: 测试🚀")
    None => @test.fail("Expected Some(body)")
  }
  
  match unicode_log.attributes[0].1 {
    AttributeValue::StringValue(unicode_val) => assert_eq(unicode_val, "Unicode值: 测试🎉")
    _ => @test.fail("Expected StringValue")
  }
}