// Azimuth Log Correlation Test Suite
// 日志关联性测试用例，专注于日志记录与追踪上下文的关联性，以及日志的严重级别和时间戳处理

test "日志记录与追踪上下文的关联性测试" {
  // 创建LoggerProvider和Logger
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "correlation_test_logger")
  
  // 创建追踪上下文
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "1234567890abcdef"
  
  // 创建带有追踪上下文的日志记录
  let trace_log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("处理用户请求开始"),
    None,  // attributes
    Some(1735689600000000000L),  // timestamp
    Some(1735689600000000000L),  // observed_timestamp
    Some(trace_id),
    Some(span_id),
    None   // context
  )
  
  // 验证日志记录的追踪关联
  assert_eq(azimuth::LogRecord::severity_number(trace_log_record), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(trace_log_record), Some("处理用户请求开始"))
  assert_eq(azimuth::LogRecord::trace_id(trace_log_record), Some(trace_id))
  assert_eq(azimuth::LogRecord::span_id(trace_log_record), Some(span_id))
  
  // 创建多个相关日志记录
  let log_record1 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("验证用户权限"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some(trace_id),
    Some(span_id),
    None
  )
  
  let log_record2 = azimuth::LogRecord::new_with_context(
    azimuth::Debug,
    Some("查询数据库"),
    None,
    Some(1735689600000001000L),
    Some(1735689600000001000L),
    Some(trace_id),
    Some(span_id),
    None
  )
  
  let log_record3 = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("响应时间较长"),
    None,
    Some(1735689600000005000L),
    Some(1735689600000005000L),
    Some(trace_id),
    Some(span_id),
    None
  )
  
  let log_record4 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("处理用户请求完成"),
    None,
    Some(1735689600000010000L),
    Some(1735689600000010000L),
    Some(trace_id),
    Some(span_id),
    None
  )
  
  // 验证所有日志记录的追踪关联
  let log_records = [log_record1, log_record2, log_record3, log_record4]
  for record in log_records {
    assert_eq(azimuth::LogRecord::trace_id(record), Some(trace_id))
    assert_eq(azimuth::LogRecord::span_id(record), Some(span_id))
  }
  
  // 验证日志严重级别
  assert_eq(azimuth::LogRecord::severity_number(log_record1), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(log_record2), azimuth::Debug)
  assert_eq(azimuth::LogRecord::severity_number(log_record3), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(log_record4), azimuth::Info)
  
  // 发出日志记录
  azimuth::Logger::emit(logger, trace_log_record)
  azimuth::Logger::emit(logger, log_record1)
  azimuth::Logger::emit(logger, log_record2)
  azimuth::Logger::emit(logger, log_record3)
  azimuth::Logger::emit(logger, log_record4)
}

test "不同严重级别日志的创建和处理测试" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "severity_test_logger")
  
  // 创建不同严重级别的日志记录
  let trace_log = azimuth::LogRecord::new(azimuth::Trace, "跟踪信息：进入函数")
  let debug_log = azimuth::LogRecord::new(azimuth::Debug, "调试信息：变量值 = 42")
  let info_log = azimuth::LogRecord::new(azimuth::Info, "信息：用户登录成功")
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, "警告：内存使用率超过80%")
  let error_log = azimuth::LogRecord::new(azimuth::Error, "错误：数据库连接失败")
  let fatal_log = azimuth::LogRecord::new(azimuth::Fatal, "致命错误：系统无法启动")
  
  // 验证日志严重级别
  assert_eq(azimuth::LogRecord::severity_number(trace_log), azimuth::Trace)
  assert_eq(azimuth::LogRecord::severity_number(debug_log), azimuth::Debug)
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(fatal_log), azimuth::Fatal)
  
  // 验证日志内容
  assert_eq(azimuth::LogRecord::body(trace_log), Some("跟踪信息：进入函数"))
  assert_eq(azimuth::LogRecord::body(debug_log), Some("调试信息：变量值 = 42"))
  assert_eq(azimuth::LogRecord::body(info_log), Some("信息：用户登录成功"))
  assert_eq(azimuth::LogRecord::body(warn_log), Some("警告：内存使用率超过80%"))
  assert_eq(azimuth::LogRecord::body(error_log), Some("错误：数据库连接失败"))
  assert_eq(azimuth::LogRecord::body(fatal_log), Some("致命错误：系统无法启动"))
  
  // 创建带有属性的日志记录
  let log_attributes = azimuth::Attributes::new()
  let log_with_attrs = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("处理请求时发生错误"),
    Some(log_attributes),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("error_trace_id"),
    Some("error_span_id"),
    None
  )
  
  assert_eq(azimuth::LogRecord::severity_number(log_with_attrs), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(log_with_attrs), Some("处理请求时发生错误"))
  
  // 发出所有日志记录
  azimuth::Logger::emit(logger, trace_log)
  azimuth::Logger::emit(logger, debug_log)
  azimuth::Logger::emit(logger, info_log)
  azimuth::Logger::emit(logger, warn_log)
  azimuth::Logger::emit(logger, error_log)
  azimuth::Logger::emit(logger, fatal_log)
  azimuth::Logger::emit(logger, log_with_attrs)
}

test "日志时间戳和时序性测试" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "timestamp_test_logger")
  
  // 创建时钟
  let clock = azimuth::Clock::system()
  
  // 获取当前时间戳
  let base_timestamp = azimuth::Clock::now_unix_nanos(clock)
  
  // 创建带有时间戳的日志记录
  let log1 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("操作开始"),
    None,
    Some(base_timestamp),
    Some(base_timestamp),
    None,
    None,
    None
  )
  
  let log2 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("操作进行中"),
    None,
    Some(base_timestamp + 1000000L),  // 1ms后
    Some(base_timestamp + 1000000L),
    None,
    None,
    None
  )
  
  let log3 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("操作结束"),
    None,
    Some(base_timestamp + 2000000L),  // 2ms后
    Some(base_timestamp + 2000000L),
    None,
    None,
    None
  )
  
  // 验证时间戳的时序性
  assert_eq(azimuth::LogRecord::body(log1), Some("操作开始"))
  assert_eq(azimuth::LogRecord::body(log2), Some("操作进行中"))
  assert_eq(azimuth::LogRecord::body(log3), Some("操作结束"))
  
  // 验证时间戳递增
  let timestamp1 = azimuth::LogRecord::timestamp(log1)
  let timestamp2 = azimuth::LogRecord::timestamp(log2)
  let timestamp3 = azimuth::LogRecord::timestamp(log3)
  
  match (timestamp1, timestamp2, timestamp3) {
    (Some(ts1), Some(ts2), Some(ts3)) => {
      assert_true(ts1 < ts2)
      assert_true(ts2 < ts3)
      assert_true(ts1 < ts3)
    }
    _ => assert_true(false)
  }
  
  // 测试相同时间戳的日志记录
  let same_time_log1 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("同时发生的事件1"),
    None,
    Some(base_timestamp + 3000000L),
    Some(base_timestamp + 3000000L),
    None,
    None,
    None
  )
  
  let same_time_log2 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("同时发生的事件2"),
    None,
    Some(base_timestamp + 3000000L),
    Some(base_timestamp + 3000000L),
    None,
    None,
    None
  )
  
  // 验证相同时间戳
  let same_timestamp1 = azimuth::LogRecord::timestamp(same_time_log1)
  let same_timestamp2 = azimuth::LogRecord::timestamp(same_time_log2)
  
  match (same_timestamp1, same_timestamp2) {
    (Some(ts1), Some(ts2)) => assert_eq(ts1, ts2)
    _ => assert_true(false)
  }
  
  // 发出所有日志记录
  azimuth::Logger::emit(logger, log1)
  azimuth::Logger::emit(logger, log2)
  azimuth::Logger::emit(logger, log3)
  azimuth::Logger::emit(logger, same_time_log1)
  azimuth::Logger::emit(logger, same_time_log2)
}

test "跨服务日志关联性测试" {
  // 创建多个服务的Logger
  let logger_provider = azimuth::LoggerProvider::default()
  
  let gateway_logger = azimuth::LoggerProvider::get_logger(logger_provider, "gateway_service")
  let auth_logger = azimuth::LoggerProvider::get_logger(logger_provider, "auth_service")
  let user_logger = azimuth::LoggerProvider::get_logger(logger_provider, "user_service")
  let order_logger = azimuth::LoggerProvider::get_logger(logger_provider, "order_service")
  
  // 共享的追踪ID
  let global_trace_id = "abcdef1234567890abcdef1234567890"
  
  // 网关服务的日志
  let gateway_log1 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("接收用户请求"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some(global_trace_id),
    Some("gateway_span_1"),
    None
  )
  
  let gateway_log2 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("转发请求到认证服务"),
    None,
    Some(1735689600000001000L),
    Some(1735689600000001000L),
    Some(global_trace_id),
    Some("gateway_span_1"),
    None
  )
  
  // 认证服务的日志
  let auth_log1 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("接收认证请求"),
    None,
    Some(1735689600000002000L),
    Some(1735689600000002000L),
    Some(global_trace_id),
    Some("auth_span_1"),
    None
  )
  
  let auth_log2 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("用户认证成功"),
    None,
    Some(1735689600000005000L),
    Some(1735689600000005000L),
    Some(global_trace_id),
    Some("auth_span_1"),
    None
  )
  
  // 用户服务的日志
  let user_log1 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("查询用户信息"),
    None,
    Some(1735689600000003000L),
    Some(1735689600000003000L),
    Some(global_trace_id),
    Some("user_span_1"),
    None
  )
  
  let user_log2 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("返回用户信息"),
    None,
    Some(1735689600000004000L),
    Some(1735689600000004000L),
    Some(global_trace_id),
    Some("user_span_1"),
    None
  )
  
  // 订单服务的日志
  let order_log1 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("创建订单"),
    None,
    Some(1735689600000006000L),
    Some(1735689600000006000L),
    Some(global_trace_id),
    Some("order_span_1"),
    None
  )
  
  let order_log2 = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("库存不足，订单创建失败"),
    None,
    Some(1735689600000008000L),
    Some(1735689600000008000L),
    Some(global_trace_id),
    Some("order_span_1"),
    None
  )
  
  // 验证所有日志都有相同的追踪ID
  let all_logs = [
    gateway_log1, gateway_log2,
    auth_log1, auth_log2,
    user_log1, user_log2,
    order_log1, order_log2
  ]
  
  for log in all_logs {
    assert_eq(azimuth::LogRecord::trace_id(log), Some(global_trace_id))
  }
  
  // 验证不同服务的Span ID不同
  assert_eq(azimuth::LogRecord::span_id(gateway_log1), Some("gateway_span_1"))
  assert_eq(azimuth::LogRecord::span_id(auth_log1), Some("auth_span_1"))
  assert_eq(azimuth::LogRecord::span_id(user_log1), Some("user_span_1"))
  assert_eq(azimuth::LogRecord::span_id(order_log1), Some("order_span_1"))
  
  // 验证日志严重级别
  assert_eq(azimuth::LogRecord::severity_number(gateway_log1), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(auth_log2), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(user_log2), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(order_log2), azimuth::Error)
  
  // 发出所有日志记录
  azimuth::Logger::emit(gateway_logger, gateway_log1)
  azimuth::Logger::emit(gateway_logger, gateway_log2)
  azimuth::Logger::emit(auth_logger, auth_log1)
  azimuth::Logger::emit(auth_logger, auth_log2)
  azimuth::Logger::emit(user_logger, user_log1)
  azimuth::Logger::emit(user_logger, user_log2)
  azimuth::Logger::emit(order_logger, order_log1)
  azimuth::Logger::emit(order_logger, order_log2)
}

test "日志属性和结构化数据测试" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "structured_log_test_logger")
  
  // 创建带有各种属性的日志记录
  let log_attributes = azimuth::Attributes::new()
  
  let structured_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("处理HTTP请求"),
    Some(log_attributes),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("structured_trace_id"),
    Some("structured_span_id"),
    None
  )
  
  // 验证结构化日志的基本属性
  assert_eq(azimuth::LogRecord::severity_number(structured_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(structured_log), Some("处理HTTP请求"))
  assert_eq(azimuth::LogRecord::trace_id(structured_log), Some("structured_trace_id"))
  assert_eq(azimuth::LogRecord::span_id(structured_log), Some("structured_span_id"))
  
  // 创建带有复杂内容的日志记录
  let complex_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("处理复杂业务逻辑时遇到异常"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("complex_trace_id"),
    Some("complex_span_id"),
    None
  )
  
  // 创建包含Unicode和特殊字符的日志记录
  let unicode_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("处理包含Unicode字符的数据时出错: 🚀 🌟 💡 🔥"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("unicode_trace_id"),
    Some("unicode_span_id"),
    None
  )
  
  // 验证Unicode日志
  assert_eq(azimuth::LogRecord::severity_number(unicode_log), azimuth::Error)
  match azimuth::LogRecord::body(unicode_log) {
    Some(body) => {
      assert_true(body.contains("🚀"))
      assert_true(body.contains("🌟"))
      assert_true(body.contains("💡"))
      assert_true(body.contains("🔥"))
    }
    None => assert_true(false)
  }
  
  // 创建包含换行符的日志记录
  let multiline_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("多行错误信息:
第一行错误描述
第二行错误描述
第三行错误描述"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("multiline_trace_id"),
    Some("multiline_span_id"),
    None
  )
  
  // 验证多行日志
  match azimuth::LogRecord::body(multiline_log) {
    Some(body) => {
      assert_true(body.contains("
"))
      assert_true(body.contains("第一行错误描述"))
      assert_true(body.contains("第二行错误描述"))
      assert_true(body.contains("第三行错误描述"))
    }
    None => assert_true(false)
  }
  
  // 创建空内容的日志记录
  let empty_body_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    None,
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("empty_body_trace_id"),
    Some("empty_body_span_id"),
    None
  )
  
  // 验证空内容日志
  assert_eq(azimuth::LogRecord::severity_number(empty_body_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(empty_body_log), None)
  
  // 发出所有日志记录
  azimuth::Logger::emit(logger, structured_log)
  azimuth::Logger::emit(logger, complex_log)
  azimuth::Logger::emit(logger, unicode_log)
  azimuth::Logger::emit(logger, multiline_log)
  azimuth::Logger::emit(logger, empty_body_log)
}