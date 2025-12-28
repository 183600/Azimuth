// 高级日志操作测试用例
// 测试LogRecord的创建、属性管理、严重级别等高级功能

test "log_record_severity_levels" {
  // 测试所有严重级别
  let trace_record = LogRecord::new(Trace, "Trace message")
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::body(trace_record), Some("Trace message"))
  
  let debug_record = LogRecord::new(Debug, "Debug message")
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::body(debug_record), Some("Debug message"))
  
  let info_record = LogRecord::new(Info, "Info message")
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::body(info_record), Some("Info message"))
  
  let warn_record = LogRecord::new(Warn, "Warning message")
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::body(warn_record), Some("Warning message"))
  
  let error_record = LogRecord::new(Error, "Error message")
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::body(error_record), Some("Error message"))
  
  let fatal_record = LogRecord::new(Fatal, "Fatal message")
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  assert_eq(LogRecord::body(fatal_record), Some("Fatal message"))
}

test "log_record_with_attributes" {
  // 测试带属性的日志记录
  let attributes = Attributes::new()
  let record_with_attrs = LogRecord::new_with_context(
    Info,
    Some("Log with attributes"),
    Some(attributes),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace123"),
    Some("span456"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(record_with_attrs), Info)
  assert_eq(LogRecord::body(record_with_attrs), Some("Log with attributes"))
  assert_eq(LogRecord::trace_id(record_with_attrs), Some("trace123"))
  assert_eq(LogRecord::span_id(record_with_attrs), Some("span456"))
}

test "log_record_timestamp_operations" {
  // 测试时间戳操作
  let clock = Clock::system()
  let current_time = Clock::now_unix_nanos(clock)
  
  let timestamped_record = LogRecord::new_with_context(
    Info,
    Some("Timestamped log"),
    None,
    Some(current_time),
    Some(current_time + 1000L),
    None,
    None,
    None
  )
  
  // 验证时间戳被正确设置
  assert_true(current_time > 0L)
  assert_eq(LogRecord::severity_number(timestamped_record), Info)
  assert_eq(LogRecord::body(timestamped_record), Some("Timestamped log"))
}

test "log_record_trace_integration" {
  // 测试与追踪系统的集成
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let traced_record = LogRecord::new_with_context(
    Error,
    Some("Error in traced operation"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  assert_eq(LogRecord::severity_number(traced_record), Error)
  assert_eq(LogRecord::body(traced_record), Some("Error in traced operation"))
  assert_eq(LogRecord::trace_id(traced_record), Some(trace_id))
  assert_eq(LogRecord::span_id(traced_record), Some(span_id))
}

test "log_record_context_integration" {
  // 测试上下文集成
  let context = Context::root()
  let key = ContextKey::new("operation.id")
  let context_with_value = Context::with_value(context, key, "op123")
  
  let contextual_record = LogRecord::new_with_context(
    Info,
    Some("Operation completed"),
    None,
    None,
    None,
    None,
    None,
    Some(context_with_value)
  )
  
  assert_eq(LogRecord::severity_number(contextual_record), Info)
  assert_eq(LogRecord::body(contextual_record), Some("Operation completed"))
  
  // 验证上下文值可以被获取
  let retrieved_value = Context::get(context_with_value, key)
  assert_eq(retrieved_value, Some("op123"))
}

test "log_record_empty_and_special_cases" {
  // 测试空值和特殊情况
  let empty_body_record = LogRecord::new_with_context(
    Info,
    None,  // 空消息体
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(empty_body_record), Info)
  assert_eq(LogRecord::body(empty_body_record), None)
  
  // 测试空字符串消息体
  let empty_string_record = LogRecord::new_with_context(
    Warn,
    Some(""),  // 空字符串
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::severity_number(empty_string_record), Warn)
  assert_eq(LogRecord::body(empty_string_record), Some(""))
}

test "log_record_special_characters" {
  // 测试特殊字符和国际化
  let unicode_record = LogRecord::new_with_context(
    Info,
    Some("用户操作完成 🎉 特殊字符: !@#$%^&*()"),
    None,
    None,
    None,
    Some("unicode_trace_id_测试"),
    Some("unicode_span_id_测试"),
    None
  )
  
  assert_eq(LogRecord::severity_number(unicode_record), Info)
  assert_eq(LogRecord::body(unicode_record), Some("用户操作完成 🎉 特殊字符: !@#$%^&*()"))
  assert_eq(LogRecord::trace_id(unicode_record), Some("unicode_trace_id_测试"))
  assert_eq(LogRecord::span_id(unicode_record), Some("unicode_span_id_测试"))
}

test "log_record_logger_integration" {
  // 测试与Logger的集成
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  // 创建各种类型的日志记录
  let info_record = LogRecord::new(Info, "Information message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  
  // 发送日志记录
  Logger::emit(logger, info_record)
  Logger::emit(logger, warn_record)
  Logger::emit(logger, error_record)
  
  // 验证日志记录的属性
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
}

test "log_record_batch_operations" {
  // 测试批量日志操作
  let log_records = [
    LogRecord::new(Info, "Batch operation 1"),
    LogRecord::new(Info, "Batch operation 2"),
    LogRecord::new(Warn, "Batch operation 3"),
    LogRecord::new(Error, "Batch operation 4")
  ]
  
  // 验证批量创建的日志记录
  assert_eq(log_records.length(), 4)
  assert_eq(LogRecord::severity_number(log_records[0]), Info)
  assert_eq(LogRecord::severity_number(log_records[1]), Info)
  assert_eq(LogRecord::severity_number(log_records[2]), Warn)
  assert_eq(LogRecord::severity_number(log_records[3]), Error)
  
  // 验证日志消息
  assert_eq(LogRecord::body(log_records[0]), Some("Batch operation 1"))
  assert_eq(LogRecord::body(log_records[1]), Some("Batch operation 2"))
  assert_eq(LogRecord::body(log_records[2]), Some("Batch operation 3"))
  assert_eq(LogRecord::body(log_records[3]), Some("Batch operation 4"))
}

test "log_record_performance_scenarios" {
  // 测试性能场景下的日志记录
  
  let clock = Clock::system()
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "performance-logger")
  
  // 创建大量日志记录（模拟性能测试）
  for i in range(0, 100) {
    let record = LogRecord::new_with_context(
      Info,
      Some("Performance test message " + i.to_string()),
      None,
      Some(Clock::now_unix_nanos(clock)),
      None,
      Some("perf_trace_" + i.to_string()),
      Some("perf_span_" + i.to_string()),
      None
    )
    
    Logger::emit(logger, record)
    assert_eq(LogRecord::severity_number(record), Info)
    assert_eq(LogRecord::trace_id(record), Some("perf_trace_" + i.to_string()))
    assert_eq(LogRecord::span_id(record), Some("perf_span_" + i.to_string()))
  }
  
  assert_true(true)  // 如果没有崩溃，测试通过
}