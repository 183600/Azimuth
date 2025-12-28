// 日志记录完整功能测试 - 测试日志记录的全面功能和特性
// Complete Logging Functionality Test - Testing comprehensive logging features and capabilities

test "基础日志记录功能测试" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "basic-test-logger")
  
  // 测试所有严重性级别
  let trace_record = LogRecord::new(Trace, "This is a trace message")
  let debug_record = LogRecord::new(Debug, "This is a debug message")
  let info_record = LogRecord::new(Info, "This is an info message")
  let warn_record = LogRecord::new(Warn, "This is a warning message")
  let error_record = LogRecord::new(Error, "This is an error message")
  let fatal_record = LogRecord::new(Fatal, "This is a fatal message")
  
  // 发送所有级别的日志
  Logger::emit(logger, trace_record)
  Logger::emit(logger, debug_record)
  Logger::emit(logger, info_record)
  Logger::emit(logger, warn_record)
  Logger::emit(logger, error_record)
  Logger::emit(logger, fatal_record)
  
  // 验证日志记录属性
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  assert_eq(LogRecord::body(trace_record), Some("This is a trace message"))
  assert_eq(LogRecord::body(debug_record), Some("This is a debug message"))
  assert_eq(LogRecord::body(info_record), Some("This is an info message"))
  assert_eq(LogRecord::body(warn_record), Some("This is a warning message"))
  assert_eq(LogRecord::body(error_record), Some("This is an error message"))
  assert_eq(LogRecord::body(fatal_record), Some("This is a fatal message"))
}

test "复杂日志记录创建测试" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "complex-test-logger")
  
  // 创建属性集合
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("12345"))
  Attributes::set(attrs, "request.id", StringValue("req-67890"))
  Attributes::set(attrs, "http.method", StringValue("POST"))
  Attributes::set(attrs, "http.status_code", IntValue(200))
  Attributes::set(attrs, "response.time_ms", IntValue(150))
  
  // 创建复杂日志记录
  let complex_record = LogRecord::new_with_context(
    Info,
    Some("User request processed successfully"),
    Some(attrs),
    Some(1735689600000000000L),  // 2025年的时间戳
    Some(1735689600000000100L),  // 观察时间戳
    Some("0af7651916cd43dd8448eb211c80319c"),  // trace_id
    Some("b7ad6b7169203331"),  // span_id
    Some(Context::root())
  )
  
  // 发送复杂日志记录
  Logger::emit(logger, complex_record)
  
  // 验证复杂日志记录的属性
  assert_eq(LogRecord::severity_number(complex_record), Info)
  assert_eq(LogRecord::body(complex_record), Some("User request processed successfully"))
  assert_eq(LogRecord::timestamp(complex_record), Some(1735689600000000000L))
  assert_eq(LogRecord::observed_timestamp(complex_record), Some(1735689600000000100L))
  assert_eq(LogRecord::trace_id(complex_record), Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(LogRecord::span_id(complex_record), Some("b7ad6b7169203331"))
}

test "日志记录属性集成测试" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "attribute-test-logger")
  
  // 测试不同类型的属性
  let string_attrs = Attributes::new()
  Attributes::set(string_attrs, "service.name", StringValue("payment-service"))
  Attributes::set(string_attrs, "service.version", StringValue("1.2.3"))
  Attributes::set(string_attrs, "deployment.environment", StringValue("production"))
  
  let mixed_attrs = Attributes::new()
  Attributes::set(mixed_attrs, "string.key", StringValue("string_value"))
  Attributes::set(mixed_attrs, "int.key", IntValue(42))
  Attributes::set(mixed_attrs, "float.key", FloatValue(3.14159))
  Attributes::set(mixed_attrs, "bool.key", BoolValue(true))
  Attributes::set(mixed_attrs, "array.string", ArrayStringValue(["value1", "value2", "value3"]))
  Attributes::set(mixed_attrs, "array.int", ArrayIntValue([1, 2, 3, 4, 5]))
  
  // 创建带有不同属性的日志记录
  let string_attr_record = LogRecord::new_with_context(
    Warn,
    Some("Service startup completed"),
    Some(string_attrs),
    None, None, None, None, None
  )
  
  let mixed_attr_record = LogRecord::new_with_context(
    Error,
    Some("Processing error occurred"),
    Some(mixed_attrs),
    None, None, None, None, None
  )
  
  // 发送日志记录
  Logger::emit(logger, string_attr_record)
  Logger::emit(logger, mixed_attr_record)
  
  // 验证属性集成
  assert_eq(LogRecord::body(string_attr_record), Some("Service startup completed"))
  assert_eq(LogRecord::body(mixed_attr_record), Some("Processing error occurred"))
  assert_eq(LogRecord::severity_number(string_attr_record), Warn)
  assert_eq(LogRecord::severity_number(mixed_attr_record), Error)
}

test "日志记录时间戳功能测试" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "timestamp-test-logger")
  
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // 测试不同时间戳场景
  let scenarios = [
    ("no_timestamp", None, None),
    ("only_timestamp", Some(base_timestamp), None),
    ("only_observed", None, Some(base_timestamp + 1000L)),
    ("both_timestamps", Some(base_timestamp), Some(base_timestamp + 1000L)),
    ("equal_timestamps", Some(base_timestamp), Some(base_timestamp))
  ]
  
  for scenario in scenarios {
    let record = LogRecord::new_with_context(
      Info,
      Some("Timestamp test: " + scenario.0),
      None,
      scenario.1,
      scenario.2,
      None,
      None,
      None
    )
    
    Logger::emit(logger, record)
    
    // 验证时间戳设置
    assert_eq(LogRecord::timestamp(record), scenario.1)
    assert_eq(LogRecord::observed_timestamp(record), scenario.2)
  }
}

test "日志记录追踪集成测试" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "tracing-test-logger")
  
  // 创建Span上下文
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "fedcba0987654321fedcba0987654321"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  
  // 测试不同追踪场景
  let tracing_scenarios = [
    ("no_tracing", None, None),
    ("trace_only", Some(trace_id), None),
    ("span_only", None, Some(span_id)),
    ("full_tracing", Some(trace_id), Some(span_id))
  ]
  
  for scenario in tracing_scenarios {
    let record = LogRecord::new_with_context(
      Debug,
      Some("Tracing test: " + scenario.0),
      None,
      None,
      None,
      scenario.1,
      scenario.2,
      None
    )
    
    Logger::emit(logger, record)
    
    // 验证追踪信息
    assert_eq(LogRecord::trace_id(record), scenario.1)
    assert_eq(LogRecord::span_id(record), scenario.2)
  }
}

test "日志记录上下文集成测试" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "context-test-logger")
  
  // 创建不同类型的上下文
  let root_ctx = Context::root()
  let key_a = ContextKey::new("context.key.a")
  let ctx_with_a = Context::with_value(root_ctx, key_a, "value.a")
  
  let key_b = ContextKey::new("context.key.b")
  let ctx_with_ab = Context::with_value(ctx_with_a, key_b, "value.b")
  
  // 测试上下文集成
  let context_scenarios = [
    ("no_context", None),
    ("root_context", Some(root_ctx)),
    ("single_context", Some(ctx_with_a)),
    ("multi_context", Some(ctx_with_ab))
  ]
  
  for scenario in context_scenarios {
    let record = LogRecord::new_with_context(
      Info,
      Some("Context test: " + scenario.0),
      None,
      None,
      None,
      None,
      None,
      scenario.1
    )
    
    Logger::emit(logger, record)
    
    // 验证上下文设置
    assert_eq(LogRecord::context(record), scenario.1)
  }
}

test "日志记录批量操作测试" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "batch-test-logger")
  
  // 创建批量日志记录
  let batch_records = []
  
  for i in 0..10 {
    let severity = match i % 5 {
      0 => Trace
      1 => Debug
      2 => Info
      3 => Warn
      _ => Error
    }
    
    let message = "Batch message " + i.to_string()
    let record = LogRecord::new(severity, message)
    batch_records.push(record)
  }
  
  // 批量发送日志记录
  for record in batch_records {
    Logger::emit(logger, record)
  }
  
  // 验证批量记录
  for i in 0..batch_records.length() {
    let expected_message = "Batch message " + i.to_string()
    assert_eq(LogRecord::body(batch_records[i]), Some(expected_message))
  }
}

test "日志记录特殊场景测试" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "special-test-logger")
  
  // 测试空消息
  let empty_message_record = LogRecord::new(Info, "")
  Logger::emit(logger, empty_message_record)
  assert_eq(LogRecord::body(empty_message_record), Some(""))
  
  // 测试None消息
  let none_message_record = LogRecord::new_with_context(
    Warn,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  Logger::emit(logger, none_message_record)
  assert_eq(LogRecord::body(none_message_record), None)
  
  // 测试长消息
  let long_message = "This is a very long log message that ".repeat(100)
  let long_message_record = LogRecord::new(Error, long_message)
  Logger::emit(logger, long_message_record)
  assert_eq(LogRecord::body(long_message_record), Some(long_message))
  
  // 测试包含特殊字符的消息
  let special_chars_message = "Special chars: !@#$%^&*(){}[]|\\:;\"'<>?,./\n\t\r"
  let special_chars_record = LogRecord::new(Fatal, special_chars_message)
  Logger::emit(logger, special_chars_record)
  assert_eq(LogRecord::body(special_chars_record), Some(special_chars_message))
  
  // 测试Unicode消息
  let unicode_message = "Unicode测试: 🚀 🔥 💯 🎉 中文 العربية 日本語 한국어"
  let unicode_record = LogRecord::new(Debug, unicode_message)
  Logger::emit(logger, unicode_record)
  assert_eq(LogRecord::body(unicode_record), Some(unicode_message))
}

test "多Logger实例测试" {
  let provider = LoggerProvider::default()
  
  // 创建多个Logger实例
  let service_logger = LoggerProvider::get_logger(provider, "payment-service")
  let auth_logger = LoggerProvider::get_logger(provider, "auth-service")
  let db_logger = LoggerProvider::get_logger(provider, "database-service")
  
  // 为每个Logger创建不同的日志记录
  let service_record = LogRecord::new(Info, "Payment processed successfully")
  let auth_record = LogRecord::new(Warn, "Authentication failed for user")
  let db_record = LogRecord::new(Error, "Database connection timeout")
  
  // 发送到不同的Logger
  Logger::emit(service_logger, service_record)
  Logger::emit(auth_logger, auth_record)
  Logger::emit(db_logger, db_record)
  
  // 验证不同Logger的记录
  assert_eq(LogRecord::body(service_record), Some("Payment processed successfully"))
  assert_eq(LogRecord::body(auth_record), Some("Authentication failed for user"))
  assert_eq(LogRecord::body(db_record), Some("Database connection timeout"))
  
  assert_eq(LogRecord::severity_number(service_record), Info)
  assert_eq(LogRecord::severity_number(auth_record), Warn)
  assert_eq(LogRecord::severity_number(db_record), Error)
}

test "日志记录性能和内存测试" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "performance-test-logger")
  
  // 创建大量日志记录测试性能
  let large_batch = []
  for i in 0..100 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    Attributes::set(attrs, "batch.id", StringValue("perf-test"))
    
    let record = LogRecord::new_with_context(
      Info,
      Some("Performance test message " + i.to_string()),
      Some(attrs),
      Some(1735689600000000000L + i.to_int64()),
      Some(1735689600000000100L + i.to_int64()),
      Some("trace" + i.to_string()),
      Some("span" + i.to_string()),
      Some(Context::root())
    )
    
    large_batch.push(record)
  }
  
  // 批量发送
  for record in large_batch {
    Logger::emit(logger, record)
  }
  
  // 验证批量处理
  assert_eq(large_batch.length(), 100)
  
  // 验证第一条和最后一条记录
  assert_eq(LogRecord::body(large_batch[0]), Some("Performance test message 0"))
  assert_eq(LogRecord::body(large_batch[99]), Some("Performance test message 99"))
  
  // 验证时间戳递增
  assert_eq(LogRecord::timestamp(large_batch[0]), Some(1735689600000000000L))
  assert_eq(LogRecord::timestamp(large_batch[99]), Some(1735689600000000099L))
}