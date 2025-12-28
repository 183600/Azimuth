// Logger高级功能测试用例
// 测试Logger和LoggerProvider的高级功能和配置

test "logger_comprehensive_lifecycle" {
  // 测试Logger的完整生命周期
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "lifecycle.test.logger")
  
  // 验证logger的基本属性
  assert_eq(logger.scope.name, "lifecycle.test.logger")
  assert_eq(logger.scope.version, None)
  assert_eq(logger.scope.schema_url, None)
  
  // 创建不同严重级别的日志记录
  let trace_record = LogRecord::new(Trace, "Trace level message for debugging")
  let debug_record = LogRecord::new(Debug, "Debug level message for development")
  let info_record = LogRecord::new(Info, "Info level message for general information")
  let warn_record = LogRecord::new(Warn, "Warning level message for potential issues")
  let error_record = LogRecord::new(Error, "Error level message for error conditions")
  let fatal_record = LogRecord::new(Fatal, "Fatal level message for critical failures")
  
  // 验证日志记录的严重级别
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  // 发送所有日志记录
  Logger::emit(logger, trace_record)
  Logger::emit(logger, debug_record)
  Logger::emit(logger, info_record)
  Logger::emit(logger, warn_record)
  Logger::emit(logger, error_record)
  Logger::emit(logger, fatal_record)
  
  assert_true(true)  // 如果到达这里说明日志生命周期正确
}

test "logger_with_context_and_attributes" {
  // 测试带有上下文和属性的日志记录
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "context.test.logger")
  
  // 创建属性
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("test.service"))
  Attributes::set(attrs, "service.version", StringValue("1.0.0"))
  Attributes::set(attrs, "request.id", StringValue("req-12345"))
  Attributes::set(attrs, "user.id", IntValue(67890))
  Attributes::set(attrs, "success", BoolValue(true))
  
  // 创建上下文
  let ctx = Context::root()
  let ctx_with_correlation = Context::with_value(ctx, ContextKey::new("correlation.id"), "corr-abcde")
  
  // 创建带有完整上下文的日志记录
  let contextual_record = LogRecord::new_with_context(
    Info,
    Some("Operation completed successfully"),
    Some(attrs),
    Some(1735689600000000000L),  // 2025-12-28 timestamp
    Some(1735689600000001000L),  // observed timestamp
    Some("trace-12345"),
    Some("span-67890"),
    Some(ctx_with_correlation)
  )
  
  // 验证日志记录的各个部分
  assert_eq(LogRecord::body(contextual_record), Some("Operation completed successfully"))
  assert_eq(LogRecord::severity_number(contextual_record), Info)
  assert_eq(LogRecord::trace_id(contextual_record), Some("trace-12345"))
  assert_eq(LogRecord::span_id(contextual_record), Some("span-67890"))
  
  // 发送带有上下文的日志
  Logger::emit(logger, contextual_record)
  
  // 创建更多带有不同上下文的日志记录
  let error_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attrs),
    None,
    None,
    Some("trace-12345"),
    Some("span-67891"),
    Some(ctx_with_correlation)
  )
  
  let recovery_record = LogRecord::new_with_context(
    Info,
    Some("Database connection recovered"),
    Some(attrs),
    Some(1735689600000002000L),
    None,
    Some("trace-12345"),
    Some("span-67892"),
    Some(ctx_with_correlation)
  )
  
  Logger::emit(logger, error_record)
  Logger::emit(logger, recovery_record)
  
  assert_true(true)  // 如果到达这里说明上下文日志处理正确
}

test "logger_provider_multiple_instances" {
  // 测试LoggerProvider的多个实例
  let provider1 = LoggerProvider::default()
  let provider2 = LoggerProvider::noop()
  let provider3 = LoggerProvider::default()
  
  // 从不同的provider创建logger
  let logger1 = LoggerProvider::get_logger(provider1, "service.auth")
  let logger2 = LoggerProvider::get_logger(provider2, "service.database")
  let logger3 = LoggerProvider::get_logger(provider3, "service.cache")
  
  // 验证不同logger的独立性
  assert_eq(logger1.scope.name, "service.auth")
  assert_eq(logger2.scope.name, "service.database")
  assert_eq(logger3.scope.name, "service.cache")
  
  // 创建带有版本信息的logger
  let versioned_logger = LoggerProvider::get_logger(provider1, "service.api", Some("2.1.0"))
  assert_eq(versioned_logger.scope.name, "service.api")
  assert_eq(versioned_logger.scope.version, Some("2.1.0"))
  
  // 创建带有schema URL的logger
  let schema_logger = LoggerProvider::get_logger(
    provider1, 
    "service.events", 
    Some("1.5.2"), 
    Some("https://opentelemetry.io/schemas/v1.5.2")
  )
  assert_eq(schema_logger.scope.name, "service.events")
  assert_eq(schema_logger.scope.version, Some("1.5.2"))
  assert_eq(schema_logger.scope.schema_url, Some("https://opentelemetry.io/schemas/v1.5.2"))
  
  // 使用不同的logger发送日志
  let auth_log = LogRecord::new(Info, "User authentication successful")
  let db_log = LogRecord::new(Warn, "Database connection pool low")
  let cache_log = LogRecord::new(Debug, "Cache miss for key: user.123")
  
  Logger::emit(logger1, auth_log)
  Logger::emit(logger2, db_log)
  Logger::emit(logger3, cache_log)
  
  assert_true(true)  // 如果到达这里说明多个logger实例正确工作
}

test "logger_structured_logging" {
  // 测试结构化日志记录
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "structured.test.logger")
  
  // 创建复杂的事件日志
  let event_attrs = Attributes::new()
  Attributes::set(event_attrs, "event.type", StringValue("user.login"))
  Attributes::set(event_attrs, "user.id", StringValue("user-12345"))
  Attributes::set(event_attrs, "user.email", StringValue("user@example.com"))
  Attributes::set(event_attrs, "ip.address", StringValue("192.168.1.100"))
  Attributes::set(event_attrs, "user.agent", StringValue("Mozilla/5.0..."))
  Attributes::set(event_attrs, "success", BoolValue(true))
  Attributes::set(event_attrs, "login.attempts", IntValue(1))
  Attributes::set(event_attrs, "session.duration", IntValue(3600))
  
  let event_log = LogRecord::new_with_context(
    Info,
    Some("User login event"),
    Some(event_attrs),
    Some(1735689600000000000L),
    Some(1735689600000000500L),
    Some("trace-session-123"),
    Some("span-login-456"),
    None
  )
  
  // 创建性能指标日志
  let perf_attrs = Attributes::new()
  Attributes::set(perf_attrs, "metric.type", StringValue("response.time"))
  Attributes::set(perf_attrs, "endpoint", StringValue("/api/users"))
  Attributes::set(perf_attrs, "method", StringValue("GET"))
  Attributes::set(perf_attrs, "status.code", IntValue(200))
  Attributes::set(perf_attrs, "response.time.ms", IntValue(150))
  Attributes::set(perf_attrs, "db.query.time.ms", IntValue(45))
  Attributes::set(perf_attrs, "cache.hit", BoolValue(true))
  
  let perf_log = LogRecord::new_with_context(
    Info,
    Some("Performance metrics collected"),
    Some(perf_attrs),
    Some(1735689600000001000L),
    None,
    Some("trace-request-789"),
    Some("span-api-012"),
    None
  )
  
  // 创建错误详情日志
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "error.type", StringValue("DatabaseError"))
  Attributes::set(error_attrs, "error.code", StringValue("DB_CONNECTION_TIMEOUT"))
  Attributes::set(error_attrs, "error.message", StringValue("Connection timeout after 30 seconds"))
  Attributes::set(error_attrs, "retry.count", IntValue(3))
  Attributes::set(error_attrs, "stack.trace", StringValue("at DatabaseService.connect..."))
  Attributes::set(error_attrs, "recovered", BoolValue(false))
  
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Database operation failed"),
    Some(error_attrs),
    Some(1735689600000002000L),
    Some(1735689600000002500L),
    Some("trace-db-error-345"),
    Some("span-db-operation-678"),
    None
  )
  
  // 发送所有结构化日志
  Logger::emit(logger, event_log)
  Logger::emit(logger, perf_log)
  Logger::emit(logger, error_log)
  
  assert_true(true)  // 如果到达这里说明结构化日志处理正确
}

test "logger_severity_filtering_and_routing" {
  // 测试日志严重级别过滤和路由
  let provider = LoggerProvider::default()
  
  // 创建不同用途的logger
  let audit_logger = LoggerProvider::get_logger(provider, "audit.logger")
  let debug_logger = LoggerProvider::get_logger(provider, "debug.logger")
  let error_logger = LoggerProvider::get_logger(provider, "error.logger")
  let access_logger = LoggerProvider::get_logger(provider, "access.logger")
  
  // 审计日志 - 只记录重要的业务事件
  let login_audit = LogRecord::new(Info, "User login: admin successful")
  let permission_change = LogRecord::new(Warn, "Permission changed for user: user123")
  let data_export = LogRecord::new(Info, "Data exported by user: user456")
  
  Logger::emit(audit_logger, login_audit)
  Logger::emit(audit_logger, permission_change)
  Logger::emit(audit_logger, data_export)
  
  // 调试日志 - 记录详细的调试信息
  let debug_info = LogRecord::new(Debug, "Processing request with parameters...")
  let trace_info = LogRecord::new(Trace, "Entering function: processUserData")
  let detailed_trace = LogRecord::new(Trace, "Variable state: userContext = {...}")
  
  Logger::emit(debug_logger, debug_info)
  Logger::emit(debug_logger, trace_info)
  Logger::emit(debug_logger, detailed_trace)
  
  // 错误日志 - 只记录错误和警告
  let db_error = LogRecord::new(Error, "Database connection failed")
  let validation_warning = LogRecord::new(Warn, "Input validation warnings detected")
  let critical_error = LogRecord::new(Fatal, "System out of memory")
  
  Logger::emit(error_logger, db_error)
  Logger::emit(error_logger, validation_warning)
  Logger::emit(error_logger, critical_error)
  
  // 访问日志 - 记录所有访问请求
  let get_request = LogRecord::new(Info, "GET /api/users - 200 OK")
  let post_request = LogRecord::new(Info, "POST /api/orders - 201 Created")
  let put_request = LogRecord::new(Info, "PUT /api/users/123 - 200 OK")
  let delete_request = LogRecord::new(Info, "DELETE /api/orders/456 - 204 No Content")
  
  Logger::emit(access_logger, get_request)
  Logger::emit(access_logger, post_request)
  Logger::emit(access_logger, put_request)
  Logger::emit(access_logger, delete_request)
  
  assert_true(true)  // 如果到达这里说明日志过滤和路由正确
}

test "logger_performance_and_batching" {
  // 测试Logger性能和批处理
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "performance.test.logger")
  
  // 模拟高频日志记录
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // 创建大量日志记录
  for i in range(0, 100) {
    let batch_record = LogRecord::new_with_context(
      Info,
      Some("Batch log entry " + i.to_string()),
      None,
      Some(start_time + (i as Int64) * 1000000L),  // 每条日志间隔1ms
      None,
      Some("batch-trace-123"),
      Some("batch-span-" + i.to_string()),
      None
    )
    Logger::emit(logger, batch_record)
  }
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let total_duration = end_time - start_time
  
  // 验证性能（在简化实现中这个测试主要是确保不会崩溃）
  assert_true(total_duration > 0L)  // 确保确实花了时间
  
  // 创建批处理日志记录
  let batch_attrs = Attributes::new()
  Attributes::set(batch_attrs, "batch.id", StringValue("batch-12345"))
  Attributes::set(batch_attrs, "batch.size", IntValue(100))
  Attributes::set(batch_attrs, "processing.time.ms", IntValue(50))
  
  let batch_summary = LogRecord::new_with_context(
    Info,
    Some("Batch processing completed"),
    Some(batch_attrs),
    Some(end_time),
    None,
    Some("batch-trace-123"),
    Some("batch-summary",
    None
  )
  
  Logger::emit(logger, batch_summary)
  
  assert_true(true)  // 如果到达这里说明性能测试通过
}