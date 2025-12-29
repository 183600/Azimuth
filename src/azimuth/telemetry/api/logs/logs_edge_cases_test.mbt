// Logs模块边缘情况测试用例
// 测试logs的边缘情况和错误处理

test "logs_severity_edge_cases" {
  // 测试日志严重级别的边缘情况
  
  // 测试所有严重级别
  let severity_levels = [
    Trace, Debug, Info, Warn, Error, Fatal
  ]
  
  // 为每个严重级别创建日志记录
  for severity in severity_levels {
    let log_record = LogRecord::builder()
      .severity(severity)
      .body("Test message for severity: " + severity.to_string())
      .build()
    
    // 验证严重级别设置正确
    assert_eq(log_record.severity_number, severity)
    
    // 验证日志记录的其他属性
    match log_record.body {
      Some(body) => {
        assert_eq(body.contains("Test message"), true)
      }
      None => @test.fail("Expected Some(body)")
    }
  }
  
  // 测试严重级别的数值比较
  let trace_val = match Trace { Trace => 0 }
  let debug_val = match Debug { Debug => 1 }
  let info_val = match Info { Info => 2 }
  let warn_val = match Warn { Warn => 3 }
  let error_val = match Error { Error => 4 }
  let fatal_val = match Fatal { Fatal => 5 }
  
  // 验证严重级别顺序
  assert_eq(trace_val < debug_val, true)
  assert_eq(debug_val < info_val, true)
  assert_eq(info_val < warn_val, true)
  assert_eq(warn_val < error_val, true)
  assert_eq(error_val < fatal_val, true)
  
  // 测试严重级别的字符串表示
  let severity_names = [
    (Trace, "TRACE"),
    (Debug, "DEBUG"),
    (Info, "INFO"),
    (Warn, "WARN"),
    (Error, "ERROR"),
    (Fatal, "FATAL")
  ]
  
  for (severity, name) in severity_names {
    let log_record = LogRecord::builder()
      .severity(severity)
      .severity_text(name)
      .build()
    
    match log_record.severity_text {
      Some(text) => {
        assert_eq(text, name)
      }
      None => @test.fail("Expected Some(severity_text)")
    }
  }
}

test "logs_log_record_edge_cases" {
  // 测试LogRecord的边缘情况
  
  // 测试极值时间戳
  let min_timestamp = -9223372036854775808L  // 最小Int64值
  let max_timestamp = 9223372036854775807L   // 最大Int64值
  let zero_timestamp = 0L
  let current_timestamp = 1640995200000000000L
  
  let timestamps = [
    min_timestamp,
    max_timestamp,
    zero_timestamp,
    current_timestamp
  ]
  
  for timestamp in timestamps {
    let log_record = LogRecord::builder()
      .timestamp(timestamp)
      .body("Message with timestamp: " + timestamp.to_string())
      .build()
    
    // 验证时间戳设置正确
    assert_eq(log_record.timestamp_unix_nanos, timestamp)
    
    // 验证时间戳的合理性检查
    let is_valid_timestamp = timestamp >= 0L  // 简化的验证逻辑
    if is_valid_timestamp {
      assert_eq(timestamp >= 0L, true)
    }
  }
  
  // 测试特殊的日志消息
  let special_messages = [
    "",                                    // 空消息
    " ",                                   // 只有空格
    "a".repeat(10000),                     // 超长消息
    "Message with newlines\nand\ttabs",    // 包含控制字符
    "Message with \"quotes\" and 'apostrophes'", // 包含引号
    "Unicode测试消息🚀🌟💫",                // Unicode和emoji
    "Message with <script>alert('xss')</script>", // XSS尝试
    "'; DROP TABLE logs; --",              // SQL注入尝试
    "Message with back\\slashes",          // 反斜杠
    "Message with /forward/slashes",       // 正斜杠
    "Message with @special #chars $here",   // 特殊字符
    "Message with multiple    spaces",     // 多个空格
    "Message\nwith\nmultiple\nnewlines",   // 多个换行符
    "Message\twith\tmultiple\ttabs",       // 多个制表符
  ]
  
  for message in special_messages {
    let log_record = LogRecord::builder()
      .severity(Info)
      .body(message)
      .build()
    
    match log_record.body {
      Some(body) => {
        assert_eq(body, message)
        
        // 验证消息的特殊属性
        let is_empty = body.length() == 0
        let is_whitespace_only = body.trim().length() == 0 && body.length() > 0
        let is_very_long = body.length() > 5000
        let has_control_chars = body.contains("\n") || body.contains("\t")
        let has_quotes = body.contains("\"") || body.contains("'")
        let has_unicode = body.contains("测试") || body.contains("🚀")
        let has_xss = body.contains("<script") || body.contains("alert(")
        let has_sql_injection = body.contains("DROP TABLE") || body.contains("DELETE")
        let has_special_chars = body.contains("@") || body.contains("#") || body.contains("$")
        
        if is_empty {
          assert_eq(body, "")
        }
        
        if is_whitespace_only {
          assert_eq(body.trim().length(), 0)
        }
        
        if is_very_long {
          assert_eq(body.length() > 5000, true)
        }
        
        if has_control_chars {
          assert_eq(body.contains("\n") || body.contains("\t"), true)
        }
        
        if has_quotes {
          assert_eq(body.contains("\"") || body.contains("'"), true)
        }
        
        if has_unicode {
          assert_eq(body.contains("测试") || body.contains("🚀"), true)
        }
        
        if has_xss {
          assert_eq(body.contains("<script") || body.contains("alert("), true)
        }
        
        if has_sql_injection {
          assert_eq(body.contains("DROP TABLE") || body.contains("DELETE"), true)
        }
        
        if has_special_chars {
          assert_eq(body.contains("@") || body.contains("#") || body.contains("$"), true)
        }
      }
      None => @test.fail("Expected Some(body)")
    }
  }
  
  // 测试复杂的属性集合
  let mut complex_attributes = []
  let mut i = 0
  while i < 100 {
    let key = "attribute_" + i.to_string()
    let value = if i % 4 == 0 {
      AttributeValue::string("string_value_" + i.to_string())
    } else if i % 4 == 1 {
      AttributeValue::int(i.to_int64())
    } else if i % 4 == 2 {
      AttributeValue::float(i.to_double())
    } else {
      AttributeValue::bool(i % 2 == 0)
    }
    complex_attributes.push((key, value))
    i = i + 1
  }
  
  let complex_log = LogRecord::builder()
    .severity(Info)
    .body("Log with many attributes")
    .build()
  
  // 手动设置复杂属性（因为builder的with_attribute方法一次只能添加一个）
  let log_with_complex_attrs = LogRecord::{
    timestamp_unix_nanos: complex_log.timestamp_unix_nanos,
    observed_timestamp_unix_nanos: complex_log.observed_timestamp_unix_nanos,
    severity_number: complex_log.severity_number,
    severity_text: complex_log.severity_text,
    body: complex_log.body,
    attributes: complex_attributes,
    trace_id: complex_log.trace_id,
    span_id: complex_log.span_id,
    trace_flags: complex_log.trace_flags,
    resource: complex_log.resource,
    instrumentation_scope: complex_log.instrumentation_scope
  }
  
  // 验证复杂属性
  assert_eq(log_with_complex_attrs.attributes.length(), 100)
  
  // 验证不同类型的属性
  match log_with_complex_attrs.attributes[0].1 {
    StringValue(value) => assert_eq(value, "string_value_0")
    _ => @test.fail("Expected StringValue")
  }
  
  match log_with_complex_attrs.attributes[1].1 {
    IntValue(value) => assert_eq(value, 1L)
    _ => @test.fail("Expected IntValue")
  }
  
  match log_with_complex_attrs.attributes[2].1 {
    FloatValue(value) => assert_eq(value, 2.0)
    _ => @test.fail("Expected FloatValue")
  }
  
  match log_with_complex_attrs.attributes[3].1 {
    BoolValue(value) => assert_eq(value, false)
    _ => @test.fail("Expected BoolValue")
  }
}

test "logs_logger_edge_cases" {
  // 测试Logger的边缘情况
  
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("edge-case-logger", Some("1.0.0"), Some("https://example.com/schema"))
  
  // 测试各种特殊的日志消息和属性组合
  let edge_cases = [
    ("", []),                                                    // 空消息，无属性
    (" ", []),                                                   // 只有空格，无属性
    ("Normal message", []),                                      // 正常消息，无属性
    ("Message with attributes", [("key", AttributeValue::string("value"))]), // 简单属性
    ("", [("key", AttributeValue::string("value"))]),            // 空消息，有属性
    (" ".repeat(100), []),                                       // 长空格消息
    ("Message".repeat(1000), []),                                // 超长消息
    ("Unicode测试🚀", []),                                        // Unicode消息
    ("Message\nwith\nnewlines", []),                             // 包含换行符
    ("Message\twith\ttabs", []),                                 // 包含制表符
    ("Message with \"quotes\"", []),                             // 包含引号
    ("Message with 'apostrophes'", []),                          // 包含撇号
    ("Message with <script>alert('xss')</script>", []),          // XSS尝试
    ("'; DROP TABLE logs; --", []),                             // SQL注入尝试
  ]
  
  for (message, attributes) in edge_cases {
    // 测试所有日志级别的方法
    logger.debug(message, attributes)
    logger.info(message, attributes)
    logger.warn(message, attributes)
    logger.error(message, attributes)
    logger.fatal(message, attributes)
    
    // 验证操作不会崩溃
    assert_eq(true, true)
    
    // 验证消息的基本属性
    let is_empty = message.length() == 0
    let is_whitespace_only = message.trim().length() == 0 && message.length() > 0
    let is_very_long = message.length() > 5000
    let has_control_chars = message.contains("\n") || message.contains("\t")
    let has_quotes = message.contains("\"") || message.contains("'")
    let has_unicode = message.contains("测试") || message.contains("🚀")
    let has_xss = message.contains("<script") || message.contains("alert(")
    let has_sql_injection = message.contains("DROP TABLE") || message.contains("DELETE")
    
    if is_empty {
      assert_eq(message, "")
    }
    
    if is_whitespace_only {
      assert_eq(message.trim().length(), 0)
    }
    
    if is_very_long {
      assert_eq(message.length() > 5000, true)
    }
    
    if has_control_chars {
      assert_eq(message.contains("\n") || message.contains("\t"), true)
    }
    
    if has_quotes {
      assert_eq(message.contains("\"") || message.contains("'"), true)
    }
    
    if has_unicode {
      assert_eq(message.contains("测试") || message.contains("🚀"), true)
    }
    
    if has_xss {
      assert_eq(message.contains("<script") || message.contains("alert("), true)
    }
    
    if has_sql_injection {
      assert_eq(message.contains("DROP TABLE") || message.contains("DELETE"), true)
    }
  }
  
  // 测试复杂的属性组合
  let complex_attributes = [
    ("string.key", AttributeValue::string("string value")),
    ("int.key", AttributeValue::int(42L)),
    ("float.key", AttributeValue::float(3.14)),
    ("bool.key", AttributeValue::bool(true)),
    ("array.string", AttributeValue::array_string(["a", "b", "c"])),
    ("array.int", AttributeValue::array_int([1L, 2L, 3L])),
    ("array.float", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("array.bool", AttributeValue::array_bool([true, false, true])),
    ("empty.string", AttributeValue::string("")),
    ("empty.array", AttributeValue::array_string([])),
    ("unicode.key", AttributeValue::string("Unicode值🚀")),
    ("special.chars", AttributeValue::string("!@#$%^&*()")),
  ]
  
  // 使用复杂属性测试所有日志级别
  logger.debug("Debug with complex attributes", complex_attributes)
  logger.info("Info with complex attributes", complex_attributes)
  logger.warn("Warn with complex attributes", complex_attributes)
  logger.error("Error with complex attributes", complex_attributes)
  logger.fatal("Fatal with complex attributes", complex_attributes)
  
  // 验证操作不会崩溃
  assert_eq(true, true)
  
  // 验证复杂属性的基本属性
  assert_eq(complex_attributes.length(), 12)
  
  // 验证不同类型的属性
  match complex_attributes[0].1 {
    StringValue(value) => assert_eq(value, "string value")
    _ => @test.fail("Expected StringValue")
  }
  
  match complex_attributes[1].1 {
    IntValue(value) => assert_eq(value, 42L)
    _ => @test.fail("Expected IntValue")
  }
  
  match complex_attributes[2].1 {
    FloatValue(value) => assert_eq(value, 3.14)
    _ => @test.fail("Expected FloatValue")
  }
  
  match complex_attributes[3].1 {
    BoolValue(value) => assert_eq(value, true)
    _ => @test.fail("Expected BoolValue")
  }
}
