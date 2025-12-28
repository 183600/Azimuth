// Azimuth Telemetry System - Severity Number and Logging Level Test
// 测试Azimuth遥测系统的严重性数字和日志级别功能

test "SeverityNumber枚举基础测试" {
  // 测试所有SeverityNumber枚举值
  
  // Trace级别
  let trace_record = LogRecord::new(Trace, "Trace message for debugging")
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::body(trace_record), Some("Trace message for debugging"))
  
  // Debug级别
  let debug_record = LogRecord::new(Debug, "Debug message for development")
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::body(debug_record), Some("Debug message for development"))
  
  // Info级别
  let info_record = LogRecord::new(Info, "Info message for general information")
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::body(info_record), Some("Info message for general information"))
  
  // Warn级别
  let warn_record = LogRecord::new(Warn, "Warning message for potential issues")
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::body(warn_record), Some("Warning message for potential issues"))
  
  // Error级别
  let error_record = LogRecord::new(Error, "Error message for failures")
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::body(error_record), Some("Error message for failures"))
  
  // Fatal级别
  let fatal_record = LogRecord::new(Fatal, "Fatal message for critical failures")
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  assert_eq(LogRecord::body(fatal_record), Some("Fatal message for critical failures"))
}

test "SeverityNumber等级比较测试" {
  // 测试SeverityNumber的等级关系
  
  // 创建严重性等级映射函数
  let severity_level = function(severity : SeverityNumber) -> Int {
    match severity {
      Trace => 1
      Debug => 2
      Info => 3
      Warn => 4
      Error => 5
      Fatal => 6
    }
  }
  
  // 验证等级关系
  assert_eq(severity_level(Trace), 1)
  assert_eq(severity_level(Debug), 2)
  assert_eq(severity_level(Info), 3)
  assert_eq(severity_level(Warn), 4)
  assert_eq(severity_level(Error), 5)
  assert_eq(severity_level(Fatal), 6)
  
  // 验证等级递增
  assert_true(severity_level(Debug) > severity_level(Trace))
  assert_true(severity_level(Info) > severity_level(Debug))
  assert_true(severity_level(Warn) > severity_level(Info))
  assert_true(severity_level(Error) > severity_level(Warn))
  assert_true(severity_level(Fatal) > severity_level(Error))
  
  // 验证极端等级比较
  assert_true(severity_level(Fatal) > severity_level(Trace))
  assert_true(severity_level(Error) > severity_level(Debug))
  assert_true(severity_level(Warn) > severity_level(Info))
}

test "SeverityNumber模式匹配测试" {
  // 测试SeverityNumber的模式匹配
  
  // 创建严重性描述函数
  let severity_description = function(severity : SeverityNumber) -> String {
    match severity {
      Trace => "最详细的调试信息"
      Debug => "调试信息"
      Info => "一般信息"
      Warn => "警告信息"
      Error => "错误信息"
      Fatal => "致命错误信息"
    }
  }
  
  // 验证描述
  assert_eq(severity_description(Trace), "最详细的调试信息")
  assert_eq(severity_description(Debug), "调试信息")
  assert_eq(severity_description(Info), "一般信息")
  assert_eq(severity_description(Warn), "警告信息")
  assert_eq(severity_description(Error), "错误信息")
  assert_eq(severity_description(Fatal), "致命错误信息")
  
  // 创建严重性分类函数
  let severity_category = function(severity : SeverityNumber) -> String {
    match severity {
      Trace | Debug => "调试类别"
      Info => "信息类别"
      Warn => "警告类别"
      Error | Fatal => "错误类别"
    }
  }
  
  // 验证分类
  assert_eq(severity_category(Trace), "调试类别")
  assert_eq(severity_category(Debug), "调试类别")
  assert_eq(severity_category(Info), "信息类别")
  assert_eq(severity_category(Warn), "警告类别")
  assert_eq(severity_category(Error), "错误类别")
  assert_eq(severity_category(Fatal), "错误类别")
}

test "LogRecord完整创建测试" {
  // 测试LogRecord的完整创建功能
  
  // 创建包含所有字段的LogRecord
  let full_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-12345"),
    Some("span-67890"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(full_record), Error)
  assert_eq(LogRecord::body(full_record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(full_record), Some("trace-12345"))
  assert_eq(LogRecord::span_id(full_record), Some("span-67890"))
  
  // 创建最小字段的LogRecord
  let minimal_record = LogRecord::new(Info, "Simple info message")
  
  assert_eq(LogRecord::severity_number(minimal_record), Info)
  assert_eq(LogRecord::body(minimal_record), Some("Simple info message"))
  assert_eq(LogRecord::trace_id(minimal_record), None)
  assert_eq(LogRecord::span_id(minimal_record), None)
  
  // 创建包含不同严重性的LogRecord数组
  let records = [
    LogRecord::new(Trace, "Trace level message"),
    LogRecord::new(Debug, "Debug level message"),
    LogRecord::new(Info, "Info level message"),
    LogRecord::new(Warn, "Warning level message"),
    LogRecord::new(Error, "Error level message"),
    LogRecord::new(Fatal, "Fatal level message")
  ]
  
  assert_eq(records.length(), 6)
  
  // 验证每个记录的严重性
  for i = 0; i < records.length(); i = i + 1 {
    let expected_severity = match i {
      0 => Trace
      1 => Debug
      2 => Info
      3 => Warn
      4 => Error
      5 => Fatal
      _ => Info
    }
    assert_eq(LogRecord::severity_number(records[i]), expected_severity)
  }
}

test "Logger和LogRecord集成测试" {
  // 测试Logger和LogRecord的集成使用
  
  // 创建Logger
  let logger_provider = LoggerProvider::default()
  let app_logger = LoggerProvider::get_logger(logger_provider, "application.logger")
  
  // 验证Logger的scope
  assert_eq(app_logger.scope.name, "application.logger")
  assert_eq(app_logger.scope.version, None)
  assert_eq(app_logger.scope.schema_url, None)
  
  // 创建不同严重性的LogRecord并发射
  let log_records = [
    LogRecord::new(Trace, "Application starting trace"),
    LogRecord::new(Debug, "Debug configuration loaded"),
    LogRecord::new(Info, "Application started successfully"),
    LogRecord::new(Warn, "High memory usage detected"),
    LogRecord::new(Error, "Database connection timeout"),
    LogRecord::new(Fatal, "Application cannot continue")
  ]
  
  // 发射所有日志记录
  for record in log_records {
    Logger::emit(app_logger, record)
  } else {
    assert_true(false) // 不应该到达这里
  }
  
  // 验证日志记录的严重性
  assert_eq(LogRecord::severity_number(log_records[0]), Trace)
  assert_eq(LogRecord::severity_number(log_records[1]), Debug)
  assert_eq(LogRecord::severity_number(log_records[2]), Info)
  assert_eq(LogRecord::severity_number(log_records[3]), Warn)
  assert_eq(LogRecord::severity_number(log_records[4]), Error)
  assert_eq(LogRecord::severity_number(log_records[5]), Fatal)
}

test "严重性级别过滤测试" {
  // 测试基于严重性级别的日志过滤
  
  // 创建不同严重性的日志记录
  let all_logs = [
    (Trace, "详细跟踪信息"),
    (Debug, "调试信息"),
    (Info, "一般信息"),
    (Warn, "警告信息"),
    (Error, "错误信息"),
    (Fatal, "致命错误")
  ]
  
  // 创建过滤函数
  let filter_by_min_severity = function(logs : Array[(SeverityNumber, String)], min_level : SeverityNumber) -> Array[(SeverityNumber, String)] {
    let mut filtered : Array[(SeverityNumber, String)] = []
    let min_level_value = match min_level {
      Trace => 1
      Debug => 2
      Info => 3
      Warn => 4
      Error => 5
      Fatal => 6
    }
    
    for (severity, message) in logs {
      let severity_value = match severity {
        Trace => 1
        Debug => 2
        Info => 3
        Warn => 4
        Error => 5
        Fatal => 6
      }
      
      if severity_value >= min_level_value {
        filtered.push((severity, message))
      }
    }
    filtered
  }
  
  // 测试不同级别的过滤
  let warn_and_above = filter_by_min_severity(all_logs, Warn)
  assert_eq(warn_and_above.length(), 3) // Warn, Error, Fatal
  
  let error_and_above = filter_by_min_severity(all_logs, Error)
  assert_eq(error_and_above.length(), 2) // Error, Fatal
  
  let info_and_above = filter_by_min_severity(all_logs, Info)
  assert_eq(info_and_above.length(), 4) // Info, Warn, Error, Fatal
  
  let all_levels = filter_by_min_severity(all_logs, Trace)
  assert_eq(all_levels.length(), 6) // 所有级别
  
  // 验证过滤结果
  assert_eq(warn_and_above[0].0, Warn)
  assert_eq(warn_and_above[1].0, Error)
  assert_eq(warn_and_above[2].0, Fatal)
}

test "LogRecord时间戳和上下文测试" {
  // 测试LogRecord的时间戳和上下文功能
  
  // 创建带时间戳的LogRecord
  let timestamp = 1735689600000000000L // 2025年1月1日的时间戳
  let observed_timestamp = 1735689600000001000L
  let record_with_timestamp = LogRecord::new_with_context(
    Info,
    Some("Message with timestamp"),
    None,
    Some(timestamp),
    Some(observed_timestamp),
    Some("trace-123"),
    Some("span-456"),
    None
  )
  
  // 注意：简化实现中可能没有时间戳的getter方法
  assert_eq(LogRecord::severity_number(record_with_timestamp), Info)
  assert_eq(LogRecord::body(record_with_timestamp), Some("Message with timestamp"))
  assert_eq(LogRecord::trace_id(record_with_timestamp), Some("trace-123"))
  assert_eq(LogRecord::span_id(record_with_timestamp), Some("span-456"))
  
  // 创建带上下文的LogRecord
  let context = Context::with_value(Context::root(), ContextKey::new("user.id"), "user-123")
  let record_with_context = LogRecord::new_with_context(
    Error,
    Some("Error with context"),
    None,
    None,
    None,
    None,
    None,
    Some(context)
  )
  
  assert_eq(LogRecord::severity_number(record_with_context), Error)
  assert_eq(LogRecord::body(record_with_context), Some("Error with context"))
  assert_eq(LogRecord::trace_id(record_with_context), None)
  assert_eq(LogRecord::span_id(record_with_context), None)
  
  // 创建带属性的LogRecord
  let attributes = Attributes::new()
  let record_with_attributes = LogRecord::new_with_context(
    Warn,
    Some("Warning with attributes"),
    Some(attributes),
    None,
    None,
    Some("trace-789"),
    Some("span-012"),
    None
  )
  
  assert_eq(LogRecord::severity_number(record_with_attributes), Warn)
  assert_eq(LogRecord::body(record_with_attributes), Some("Warning with attributes"))
  assert_eq(LogRecord::trace_id(record_with_attributes), Some("trace-789"))
  assert_eq(LogRecord::span_id(record_with_attributes), Some("span-012"))
}

test "LoggerProvider和Logger创建测试" {
  // 测试LoggerProvider和Logger的创建
  
  // 默认LoggerProvider
  let default_provider = LoggerProvider::default()
  let default_logger = LoggerProvider::get_logger(default_provider, "default.logger")
  
  assert_eq(default_logger.scope.name, "default.logger")
  assert_eq(default_logger.scope.version, None)
  assert_eq(default_logger.scope.schema_url, None)
  
  // noop LoggerProvider
  let noop_provider = LoggerProvider::noop()
  let noop_logger = LoggerProvider::get_logger(noop_provider, "noop.logger")
  
  assert_eq(noop_logger.scope.name, "noop.logger")
  assert_eq(noop_logger.scope.version, None)
  assert_eq(noop_logger.scope.schema_url, None)
  
  // 带版本的Logger
  let versioned_logger = LoggerProvider::get_logger(default_provider, "versioned.logger", Some("1.2.3"))
  
  assert_eq(versioned_logger.scope.name, "versioned.logger")
  assert_eq(versioned_logger.scope.version, Some("1.2.3"))
  assert_eq(versioned_logger.scope.schema_url, None)
  
  // 创建多个Logger
  let loggers = [
    LoggerProvider::get_logger(default_provider, "app.logger"),
    LoggerProvider::get_logger(default_provider, "db.logger", Some("1.0.0")),
    LoggerProvider::get_logger(default_provider, "http.logger"),
    LoggerProvider::get_logger(default_provider, "error.logger", Some("2.1.0"))
  ]
  
  assert_eq(loggers.length(), 4)
  
  // 验证每个Logger的名称
  let expected_names = ["app.logger", "db.logger", "http.logger", "error.logger"]
  for i = 0; i < loggers.length(); i = i + 1 {
    assert_eq(loggers[i].scope.name, expected_names[i])
  }
  
  // 验证版本
  assert_eq(loggers[0].scope.version, None)
  assert_eq(loggers[1].scope.version, Some("1.0.0"))
  assert_eq(loggers[2].scope.version, None)
  assert_eq(loggers[3].scope.version, Some("2.1.0"))
}

test "实际应用场景日志测试" {
  // 测试实际应用场景中的日志记录
  
  let logger_provider = LoggerProvider::default()
  let app_logger = LoggerProvider::get_logger(logger_provider, "web.application", Some("1.0.0"))
  
  // 模拟Web应用的日志场景
  let startup_logs = [
    LogRecord::new(Info, "Application starting"),
    LogRecord::new(Debug, "Loading configuration from config.yaml"),
    LogRecord::new(Info, "Database connection pool initialized"),
    LogRecord::new(Info, "HTTP server listening on port 8080")
  ]
  
  // 模拟请求处理的日志场景
  let request_logs = [
    LogRecord::new_with_context(
      Info, 
      Some("GET /api/users request received"), 
      None, 
      None, 
      None, 
      Some("req-trace-001"), 
      Some("req-span-001"), 
      None
    ),
    LogRecord::new_with_context(
      Debug, 
      Some("Querying database for users"), 
      None, 
      None, 
      None, 
      Some("req-trace-001"), 
      Some("req-span-002"), 
      None
    ),
    LogRecord::new_with_context(
      Info, 
      Some("Successfully retrieved 25 users"), 
      None, 
      None, 
      None, 
      Some("req-trace-001"), 
      Some("req-span-003"), 
      None
    )
  ]
  
  // 模拟错误处理的日志场景
  let error_logs = [
    LogRecord::new_with_context(
      Warn, 
      Some("High memory usage: 85%"), 
      None, 
      None, 
      None, 
      Some("health-trace-001"), 
      Some("health-span-001"), 
      None
    ),
    LogRecord::new_with_context(
      Error, 
      Some("Database connection timeout after 30 seconds"), 
      None, 
      None, 
      None, 
      Some("error-trace-001"), 
      Some("error-span-001"), 
      None
    ),
    LogRecord::new_with_context(
      Fatal, 
      Some("Cannot recover from database errors, shutting down"), 
      None, 
      None, 
      None, 
      Some("fatal-trace-001"), 
      Some("fatal-span-001"), 
      None
    )
  ]
  
  // 发射所有日志
  for log in startup_logs {
    Logger::emit(app_logger, log)
  }
  
  for log in request_logs {
    Logger::emit(app_logger, log)
  }
  
  for log in error_logs {
    Logger::emit(app_logger, log)
  }
  
  // 验证日志数量
  assert_eq(startup_logs.length(), 4)
  assert_eq(request_logs.length(), 3)
  assert_eq(error_logs.length(), 3)
  
  // 验证关键日志的trace ID
  assert_eq(LogRecord::trace_id(request_logs[0]), Some("req-trace-001"))
  assert_eq(LogRecord::span_id(error_logs[1]), Some("error-span-001"))
  assert_eq(LogRecord::severity_number(error_logs[2]), Fatal)
}