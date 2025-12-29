test "log_record_edge_cases" {
  // 测试LogRecord的边界情况
  
  // 测试空body
  let empty_body_log = LogRecord::builder()
    .severity(Info)
    .body("")
    .build()
  
  match empty_body_log.body {
    Some(body) => assert_eq(body, "")
    None => @test.fail("Test failed")
  }
  
  // 测试None body
  let none_body_log = LogRecord::builder()
    .severity(Error)
    .build()
  
  match none_body_log.body {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 测试极长body
  let long_body = "a".repeat(1000)
  let long_body_log = LogRecord::builder()
    .severity(Warn)
    .body(long_body)
    .build()
  
  match long_body_log.body {
    Some(body) => assert_eq(body.length(), 1000)
    None => @test.fail("Test failed")
  }
}

test "log_record_with_special_characters" {
  // 测试包含特殊字符的日志记录
  
  let special_chars_log = LogRecord::builder()
    .severity(Info)
    .body("Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?")
    .with_attribute("unicode", AttributeValue::string("测试中文🚀"))
    .with_attribute("newlines", AttributeValue::string("line1\nline2\rline3"))
    .with_attribute("tabs", AttributeValue::string("col1\tcol2\tcol3"))
    .build()
  
  match special_chars_log.body {
    Some(body) => {
      assert_eq(body.contains("!@#$%^&*()"), true)
      assert_eq(body.length(), 38)
    }
    None => @test.fail("Test failed")
  }
  
  assert_eq(special_chars_log.attributes.length(), 3)
  
  // 验证Unicode属性
  match special_chars_log.attributes[0].1 {
    AttributeValue::StringValue(unicode) => {
      assert_eq(unicode.contains("测试"), true)
      assert_eq(unicode.contains("🚀"), true)
    }
    _ => @test.fail("Test failed")
  }
}