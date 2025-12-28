// 日志记录完整性测试用例
// 测试日志记录的完整性，包括各种严重级别、属性和上下文信息

test "log_record_basic_integrity" {
  // 测试日志记录的基本完整性
  let log_record = LogRecord::new(Info, "Test log message")
  
  // 验证严重级别
  assert_eq(LogRecord::severity_number(log_record), Info)
  
  // 验证消息体
  let body = LogRecord::body(log_record)
  match body {
    Some(message) => assert_eq(message, "Test log message")
    _ => assert_false(true, "Expected log body")
  }
  
  // 验证默认未设置的字段
  match LogRecord::trace_id(log_record) {
    Some(_) => assert_false(true, "Expected no trace_id")
    _ => ()  // 期望None
  }
  
  match LogRecord::span_id(log_record) {
    Some(_) => assert_false(true, "Expected no span_id")
    _ => ()  // 期望None
  }
}

test "log_record_all_severity_levels" {
  // 测试所有严重级别的日志记录
  let severity_messages = [
    (Trace, "Trace level message"),
    (Debug, "Debug level message"),
    (Info, "Info level message"),
    (Warn, "Warning level message"),
    (Error, "Error level message"),
    (Fatal, "Fatal level message")
  ]
  
  for (severity, message) in severity_messages {
    let log_record = LogRecord::new(severity, message)
    
    // 验证严重级别
    assert_eq(LogRecord::severity_number(log_record), severity)
    
    // 验证消息体
    let body = LogRecord::body(log_record)
    match body {
      Some(msg) => assert_eq(msg, message)
      _ => assert_false(true, "Expected log body for severity " + severity.to_string())
    }
  }
}

test "log_record_with_full_context" {
  // 测试带完整上下文的日志记录
  let attributes = Attributes::new()
  Attributes::set(attributes, "user.id", StringValue("12345"))
  Attributes::set(attributes, "request.id", StringValue("req-67890"))
  
  let timestamp = 1735689600000000000L  // 2025年的时间戳
  let observed_timestamp = 1735689600000001000L
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let context = Context::with_value(Context::root(), ContextKey::new("correlation.id"), "corr-123")
  
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attributes),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(context)
  )
  
  // 验证严重级别
  assert_eq(LogRecord::severity_number(log_record), Error)
  
  // 验证消息体
  let body = LogRecord::body(log_record)
  match body {
    Some(message) => assert_eq(message, "Database connection failed")
    _ => assert_false(true, "Expected error message")
  }
  
  // 验证跟踪上下文
  let trace_id_result = LogRecord::trace_id(log_record)
  match trace_id_result {
    Some(id) => assert_eq(id, trace_id)
    _ => assert_false(true, "Expected trace_id")
  }
  
  let span_id_result = LogRecord::span_id(log_record)
  match span_id_result {
    Some(id) => assert_eq(id, span_id)
    _ => assert_false(true, "Expected span_id")
  }
}

test "log_record_with_various_body_types" {
  // 测试各种消息体类型的日志记录
  let test_cases = [
    ("Empty message", ""),
    ("Single word", "Hello"),
    ("Special characters", "特殊字符_测试!@#$%"),
    ("JSON like", "{\"key\": \"value\", \"number\": 42}"),
    ("Long message", "This is a very long log message that contains multiple words and should be handled properly by the logging system without any issues or truncation")
  ]
  
  for (description, message) in test_cases {
    let log_record = LogRecord::new(Info, message)
    
    let body = LogRecord::body(log_record)
    match body {
      Some(msg) => {
        assert_eq(msg, message)
        assert_eq(msg.length(), message.length())
      }
      _ => assert_false(true, "Expected log body for " + description)
    }
  }
}

test "log_record_with_complex_attributes" {
  // 测试带复杂属性的日志记录
  let attributes = Attributes::new()
  
  // 设置各种类型的属性
  Attributes::set(attributes, "string.attr", StringValue("string value"))
  Attributes::set(attributes, "int.attr", IntValue(42))
  Attributes::set(attributes, "float.attr", FloatValue(3.14159))
  Attributes::set(attributes, "bool.attr", BoolValue(true))
  Attributes::set(attributes, "string.array", ArrayStringValue(["item1", "item2", "item3"]))
  Attributes::set(attributes, "int.array", ArrayIntValue([1, 2, 3, 4, 5]))
  
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Log with complex attributes"),
    Some(attributes),
    None,
    None,
    None,
    None,
    None
  )
  
  // 验证消息体
  let body = LogRecord::body(log_record)
  match body {
    Some(message) => assert_eq(message, "Log with complex attributes")
    _ => assert_false(true, "Expected log body")
  }
}

test "log_record_timestamp_integrity" {
  // 测试日志记录时间戳完整性
  let base_timestamp = 1735689600000000000L
  let time_increment = 1000000L  // 1毫秒
  
  for i in 0..10 {
    let timestamp = base_timestamp + (i * time_increment)
    let observed_timestamp = timestamp + 1000L  // 观察时间戳稍晚
    
    let log_record = LogRecord::new_with_context(
      Info,
      Some("Timestamp test " + i.to_string()),
      None,
      Some(timestamp),
      Some(observed_timestamp),
      None,
      None,
      None
    )
    
    // 验证时间戳设置正确
    match log_record.timestamp {
      Some(ts) => assert_eq(ts, timestamp)
      _ => assert_false(true, "Expected timestamp for record " + i.to_string())
    }
    
    match log_record.observed_timestamp {
      Some(ots) => assert_eq(ots, observed_timestamp)
      _ => assert_false(true, "Expected observed_timestamp for record " + i.to_string())
    }
  }
}

test "logger_provider_and_logger_creation" {
  // 测试日志提供者和日志记录器创建
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  // 验证日志记录器属性
  assert_eq(logger.scope.name, "test-logger")
  match logger.scope.version {
    Some(_) => assert_false(true, "Expected no version for default logger")
    _ => ()  // 期望None
  }
  match logger.scope.schema_url {
    Some(_) => assert_false(true, "Expected no schema_url for default logger")
    _ => ()  // 期望None
  }
  
  // 测试日志记录器发射日志
  let log_record = LogRecord::new(Info, "Test log emission")
  Logger::emit(logger, log_record)
  
  // 验证日志记录完整性
  assert_eq(LogRecord::severity_number(log_record), Info)
  let body = LogRecord::body(log_record)
  match body {
    Some(message) => assert_eq(message, "Test log emission")
    _ => assert_false(true, "Expected log body after emission")
  }
}

test "noop_logger_operations" {
  // 测试noop日志记录器操作
  let noop_provider = LoggerProvider::noop()
  let noop_logger = LoggerProvider::get_logger(noop_provider, "noop-logger")
  
  // 验证noop日志记录器属性
  assert_eq(noop_logger.scope.name, "noop-logger")
  
  // 测试noop日志记录器发射日志
  let log_record = LogRecord::new(Error, "Noop log message")
  Logger::emit(noop_logger, log_record)
  
  // 验证日志记录完整性（即使noop也应该保持数据完整性）
  assert_eq(LogRecord::severity_number(log_record), Error)
  let body = LogRecord::body(log_record)
  match body {
    Some(message) => assert_eq(message, "Noop log message")
    _ => assert_false(true, "Expected log body for noop logger")
  }
}

test "log_record_context_preservation" {
  // 测试日志记录上下文保持
  let base_context = Context::with_value(Context::root(), ContextKey::new("request.id"), "req-12345")
  let enriched_context = Context::with_value(base_context, ContextKey::new("user.id"), "user-67890")
  let final_context = Context::with_value(enriched_context, ContextKey::new("session.id"), "session-abc")
  
  let log_record = LogRecord::new_with_context(
    Warn,
    Some("Context preservation test"),
    None,
    None,
    None,
    None,
    None,
    Some(final_context)
  )
  
  // 验证上下文保持
  match log_record.context {
    Some(ctx) => {
      let request_id = Context::get(ctx, ContextKey::new("request.id"))
      match request_id {
        Some(id) => assert_eq(id, "req-12345")
        _ => assert_false(true, "Expected request.id in context")
      }
      
      let user_id = Context::get(ctx, ContextKey::new("user.id"))
      match user_id {
        Some(id) => assert_eq(id, "user-67890")
        _ => assert_false(true, "Expected user.id in context")
      }
      
      let session_id = Context::get(ctx, ContextKey::new("session.id"))
      match session_id {
        Some(id) => assert_eq(id, "session-abc")
        _ => assert_false(true, "Expected session.id in context")
      }
    }
    _ => assert_false(true, "Expected context in log record")
  }
}

test "log_record_integrity_under_stress" {
  // 测试压力下的日志记录完整性
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "stress-test-logger")
  
  // 创建大量日志记录
  for i in 0..100 {
    let severity = match i % 6 {
      0 => Trace
      1 => Debug
      2 => Info
      3 => Warn
      4 => Error
      _ => Fatal
    }
    
    let message = "Stress test log message " + i.to_string()
    let log_record = LogRecord::new(severity, message)
    
    // 发射日志记录
    Logger::emit(logger, log_record)
    
    // 验证每条记录的完整性
    assert_eq(LogRecord::severity_number(log_record), severity)
    let body = LogRecord::body(log_record)
    match body {
      Some(msg) => {
        assert_eq(msg, message)
        assert_true(msg.contains("Stress test log message"))
        assert_true(msg.contains(i.to_string()))
      }
      _ => assert_false(true, "Expected log body for stress test " + i.to_string())
    }
  }
}