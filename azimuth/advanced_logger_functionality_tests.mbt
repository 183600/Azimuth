// 高级日志记录器功能测试用例
// 测试日志记录器的高级功能和复杂场景

pub test "logger with multiple severity levels" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "multi-level-test-logger")
  
  // 测试所有严重级别的日志记录
  let trace_log = azimuth::LogRecord::new(azimuth::Trace, "Trace level message: Detailed debugging info")
  let debug_log = azimuth::LogRecord::new(azimuth::Debug, "Debug level message: Development debugging info")
  let info_log = azimuth::LogRecord::new(azimuth::Info, "Info level message: General information")
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, "Warn level message: Warning condition")
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Error level message: Error condition")
  let fatal_log = azimuth::LogRecord::new(azimuth::Fatal, "Fatal level message: Fatal error condition")
  
  // 验证严重级别
  assert_eq(azimuth::LogRecord::severity_number(trace_log), azimuth::Trace)
  assert_eq(azimuth::LogRecord::severity_number(debug_log), azimuth::Debug)
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(fatal_log), azimuth::Fatal)
  
  // 发射所有级别的日志
  azimuth::Logger::emit(logger, trace_log)
  azimuth::Logger::emit(logger, debug_log)
  azimuth::Logger::emit(logger, info_log)
  azimuth::Logger::emit(logger, warn_log)
  azimuth::Logger::emit(logger, error_log)
  azimuth::Logger::emit(logger, fatal_log)
  
  // 验证日志消息内容
  assert_eq(azimuth::LogRecord::body(trace_log), Some("Trace level message: Detailed debugging info"))
  assert_eq(azimuth::LogRecord::body(debug_log), Some("Debug level message: Development debugging info"))
  assert_eq(azimuth::LogRecord::body(info_log), Some("Info level message: General information"))
  assert_eq(azimuth::LogRecord::body(warn_log), Some("Warn level message: Warning condition"))
  assert_eq(azimuth::LogRecord::body(error_log), Some("Error level message: Error condition"))
  assert_eq(azimuth::LogRecord::body(fatal_log), Some("Fatal level message: Fatal error condition"))
}

pub test "logger with rich contextual information" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "contextual-test-logger")
  
  // 创建丰富的上下文属性
  let context_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(context_attrs, "user.id", azimuth::StringValue("user-12345"))
  azimuth::Attributes::set(context_attrs, "user.role", azimuth::StringValue("admin"))
  azimuth::Attributes::set(context_attrs, "session.id", azimuth::StringValue("session-67890"))
  azimuth::Attributes::set(context_attrs, "request.id", azimuth::StringValue("req-abc123"))
  azimuth::Attributes::set(context_attrs, "client.ip", azimuth::StringValue("192.168.1.100"))
  azimuth::Attributes::set(context_attrs, "user.agent", azimuth::StringValue("Azimuth-Client/1.0.0"))
  azimuth::Attributes::set(context_attrs, "request.method", azimuth::StringValue("POST"))
  azimuth::Attributes::set(context_attrs, "request.path", azimuth::StringValue("/api/v1/users"))
  azimuth::Attributes::set(context_attrs, "response.status", azimuth::IntValue(201))
  
  // 创建带有丰富上下文的日志记录
  let contextual_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("User operation completed successfully"),
    Some(context_attrs),
    Some(1735689600000000000L), // 2025-12-28T14:30:00Z
    Some(1735689600000000001L), // 观察时间戳
    Some("trace-user-operation-123"),
    Some("span-user-operation-456"),
    Some(azimuth::Context::root())
  )
  
  // 验证日志记录的所有属性
  assert_eq(azimuth::LogRecord::severity_number(contextual_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(contextual_log), Some("User operation completed successfully"))
  assert_ne(azimuth::LogRecord::attributes(contextual_log), None)
  assert_eq(azimuth::LogRecord::timestamp(contextual_log), Some(1735689600000000000L))
  assert_eq(azimuth::LogRecord::observed_timestamp(contextual_log), Some(1735689600000000001L))
  assert_eq(azimuth::LogRecord::trace_id(contextual_log), Some("trace-user-operation-123"))
  assert_eq(azimuth::LogRecord::span_id(contextual_log), Some("span-user-operation-456"))
  assert_ne(azimuth::LogRecord::context(contextual_log), None)
  
  // 发射日志
  azimuth::Logger::emit(logger, contextual_log)
}

pub test "logger with structured data and events" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "structured-test-logger")
  
  // 创建结构化数据属性
  let structured_attrs = azimuth::Attributes::new()
  
  // 用户信息
  azimuth::Attributes::set(structured_attrs, "user.id", azimuth::StringValue("user-123"))
  azimuth::Attributes::set(structured_attrs, "user.name", azimuth::StringValue("John Doe"))
  azimuth::Attributes::set(structured_attrs, "user.email", azimuth::StringValue("john@example.com"))
  azimuth::Attributes::set(structured_attrs, "user.age", azimuth::IntValue(30))
  azimuth::Attributes::set(structured_attrs, "user.active", azimuth::BoolValue(true))
  azimuth::Attributes::set(structured_attrs, "user.roles", azimuth::ArrayStringValue(["user", "admin", "moderator"]))
  
  // 请求信息
  azimuth::Attributes::set(structured_attrs, "request.id", azimuth::StringValue("req-456"))
  azimuth::Attributes::set(structured_attrs, "request.method", azimuth::StringValue("PUT"))
  azimuth::Attributes::set(structured_attrs, "request.endpoint", azimuth::StringValue("/api/v1/users/123"))
  azimuth::Attributes::set(structured_attrs, "request.size", azimuth::IntValue(1024))
  azimuth::Attributes::set(structured_attrs, "request.duration", azimuth::FloatValue(150.5))
  
  // 响应信息
  azimuth::Attributes::set(structured_attrs, "response.status", azimuth::IntValue(200))
  azimuth::Attributes::set(structured_attrs, "response.size", azimuth::IntValue(512))
  azimuth::Attributes::set(structured_attrs, "response.headers", azimuth::ArrayStringValue(["Content-Type: application/json", "Cache-Control: no-cache"]))
  
  // 系统信息
  azimuth::Attributes::set(structured_attrs, "system.host", azimuth::StringValue("server-001"))
  azimuth::Attributes::set(structured_attrs, "system.region", azimuth::StringValue("us-west-2"))
  azimuth::Attributes::set(structured_attrs, "system.version", azimuth::StringValue("1.2.3"))
  
  // 创建结构化日志记录
  let structured_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Structured user update operation"),
    Some(structured_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-structured-123"),
    Some("span-structured-456"),
    Some(azimuth::Context::root())
  )
  
  // 验证结构化数据
  assert_ne(azimuth::LogRecord::attributes(structured_log), None)
  assert_eq(azimuth::LogRecord::body(structured_log), Some("Structured user update operation"))
  
  // 发射日志
  azimuth::Logger::emit(logger, structured_log)
}

pub test "logger with error and exception handling" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "error-handling-test-logger")
  
  // 创建错误相关的属性
  let error_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(error_attrs, "error.type", azimuth::StringValue("DatabaseConnectionError"))
  azimuth::Attributes::set(error_attrs, "error.message", azimuth::StringValue("Failed to connect to database: Connection timeout"))
  azimuth::Attributes::set(error_attrs, "error.code", azimuth::IntValue(5001))
  azimuth::Attributes::set(error_attrs, "error.stack_trace", azimuth::StringValue("at Database.connect (db.js:123)\nat UserService.createUser (user.js:456)\nat UserController.create (controller.js:789)"))
  azimuth::Attributes::set(error_attrs, "error.retry_count", azimuth::IntValue(3))
  azimuth::Attributes::set(error_attrs, "error.retryable", azimuth::BoolValue(true))
  
  // 数据库连接详情
  azimuth::Attributes::set(error_attrs, "database.host", azimuth::StringValue("db-primary.example.com"))
  azimuth::Attributes::set(error_attrs, "database.port", azimuth::IntValue(5432))
  azimuth::Attributes::set(error_attrs, "database.name", azimuth::StringValue("production_db"))
  azimuth::Attributes::set(error_attrs, "database.timeout", azimuth::IntValue(30))
  
  // 创建错误日志记录
  let error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Database operation failed"),
    Some(error_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-error-123"),
    Some("span-error-456"),
    Some(azimuth::Context::root())
  )
  
  // 验证错误日志
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(error_log), Some("Database operation failed"))
  assert_ne(azimuth::LogRecord::attributes(error_log), None)
  
  // 发射错误日志
  azimuth::Logger::emit(logger, error_log)
  
  // 创建严重错误日志
  let fatal_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(fatal_attrs, "error.type", azimuth::StringValue("OutOfMemoryError"))
  azimuth::Attributes::set(fatal_attrs, "error.message", azimuth::StringValue("JVM heap space exhausted"))
  azimuth::Attributes::set(fatal_attrs, "system.memory.used", azimuth::IntValue(8192))
  azimuth::Attributes::set(fatal_attrs, "system.memory.total", azimuth::IntValue(8192))
  azimuth::Attributes::set(fatal_attrs, "system.memory.percentage", azimuth::FloatValue(100.0))
  
  let fatal_log = azimuth::LogRecord::new_with_context(
    azimuth::Fatal,
    Some("Critical system error: Out of memory"),
    Some(fatal_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-fatal-789"),
    Some("span-fatal-012"),
    Some(azimuth::Context::root())
  )
  
  // 验证严重错误日志
  assert_eq(azimuth::LogRecord::severity_number(fatal_log), azimuth::Fatal)
  assert_eq(azimuth::LogRecord::body(fatal_log), Some("Critical system error: Out of memory"))
  
  // 发射严重错误日志
  azimuth::Logger::emit(logger, fatal_log)
}

pub test "logger with performance and timing data" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "performance-test-logger")
  
  // 创建性能相关的属性
  let perf_attrs = azimuth::Attributes::new()
  
  // 操作时间
  azimuth::Attributes::set(perf_attrs, "operation.name", azimuth::StringValue("user.profile.update"))
  azimuth::Attributes::set(perf_attrs, "operation.start_time", azimuth::IntValue(1735689600000L))
  azimuth::Attributes::set(perf_attrs, "operation.end_time", azimuth::IntValue(1735689600150L))
  azimuth::Attributes::set(perf_attrs, "operation.duration_ms", azimuth::IntValue(150))
  azimuth::Attributes::set(perf_attrs, "operation.duration_ns", azimuth::IntValue(150000000L))
  
  // 数据库查询性能
  azimuth::Attributes::set(perf_attrs, "db.query.count", azimuth::IntValue(5))
  azimuth::Attributes::set(perf_attrs, "db.query.total_time_ms", azimuth::IntValue(45))
  azimuth::Attributes::set(perf_attrs, "db.query.avg_time_ms", azimuth::FloatValue(9.0))
  azimuth::Attributes::set(perf_attrs, "db.query.slow_count", azimuth::IntValue(1))
  azimuth::Attributes::set(perf_attrs, "db.query.slow_threshold_ms", azimuth::IntValue(20))
  
  // 缓存性能
  azimuth::Attributes::set(perf_attrs, "cache.hits", azimuth::IntValue(12))
  azimuth::Attributes::set(perf_attrs, "cache.misses", azimuth::IntValue(3))
  azimuth::Attributes::set(perf_attrs, "cache.hit_ratio", azimuth::FloatValue(0.8))
  azimuth::Attributes::set(perf_attrs, "cache.total_time_ms", azimuth::IntValue(5))
  
  // 网络性能
  azimuth::Attributes::set(perf_attrs, "network.request_size_bytes", azimuth::IntValue(1024))
  azimuth::Attributes::set(perf_attrs, "network.response_size_bytes", azimuth::IntValue(2048))
  azimuth::Attributes::set(perf_attrs, "network.roundtrip_time_ms", azimuth::IntValue(25))
  azimuth::Attributes::set(perf_attrs, "network.bandwidth_kbps", azimuth::FloatValue(81.92))
  
  // 系统资源使用
  azimuth::Attributes::set(perf_attrs, "system.cpu.usage_percent", azimuth::FloatValue(45.5))
  azimuth::Attributes::set(perf_attrs, "system.memory.used_mb", azimuth::IntValue(2048))
  azimuth::Attributes::set(perf_attrs, "system.memory.available_mb", azimuth::IntValue(6144))
  azimuth::Attributes::set(perf_attrs, "system.disk.read_ops", azimuth::IntValue(100))
  azimuth::Attributes::set(perf_attrs, "system.disk.write_ops", azimuth::IntValue(50))
  
  // 创建性能日志记录
  let perf_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Performance metrics for user profile update"),
    Some(perf_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-perf-123"),
    Some("span-perf-456"),
    Some(azimuth::Context::root())
  )
  
  // 验证性能日志
  assert_eq(azimuth::LogRecord::severity_number(perf_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(perf_log), Some("Performance metrics for user profile update"))
  assert_ne(azimuth::LogRecord::attributes(perf_log), None)
  
  // 发射性能日志
  azimuth::Logger::emit(logger, perf_log)
}

pub test "logger with security and audit events" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "security-audit-logger")
  
  // 创建安全审计相关的属性
  let security_attrs = azimuth::Attributes::new()
  
  // 认证信息
  azimuth::Attributes::set(security_attrs, "auth.user_id", azimuth::StringValue("user-123"))
  azimuth::Attributes::set(security_attrs, "auth.username", azimuth::StringValue("john.doe"))
  azimuth::Attributes::set(security_attrs, "auth.method", azimuth::StringValue("oauth2"))
  azimuth::Attributes::set(security_attrs, "auth.provider", azimuth::StringValue("google"))
  azimuth::Attributes::set(security_attrs, "auth.success", azimuth::BoolValue(true))
  azimuth::Attributes::set(security_attrs, "auth.timestamp", azimuth::IntValue(1735689600L))
  
  // 授权信息
  azimuth::Attributes::set(security_attrs, "authz.resource", azimuth::StringValue("/api/v1/admin/users"))
  azimuth::Attributes::set(security_attrs, "authz.action", azimuth::StringValue("READ"))
  azimuth::Attributes::set(security_attrs, "authz.permission", azimuth::StringValue("users.read"))
  azimuth::Attributes::set(security_attrs, "authz.granted", azimuth::BoolValue(true))
  azimuth::Attributes::set(security_attrs, "authz.policy", azimuth::StringValue("role-based-access-control"))
  
  // 会话信息
  azimuth::Attributes::set(security_attrs, "session.id", azimuth::StringValue("sess-abc123"))
  azimuth::Attributes::set(security_attrs, "session.created_at", azimuth::IntValue(1735689000L))
  azimuth::Attributes::set(security_attrs, "session.expires_at", azimuth::IntValue(1735775400L))
  azimuth::Attributes::set(security_attrs, "session.ip_address", azimuth::StringValue("203.0.113.1"))
  azimuth::Attributes::set(security_attrs, "session.user_agent", azimuth::StringValue("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"))
  
  // 安全事件详情
  azimuth::Attributes::set(security_attrs, "security.event_type", azimuth::StringValue("USER_LOGIN"))
  azimuth::Attributes::set(security_attrs, "security.risk_score", azimuth::FloatValue(0.2))
  azimuth::Attributes::set(security_attrs, "security.anomaly_detected", azimuth::BoolValue(false))
  azimuth::Attributes::set(security_attrs, "security.mfa_required", azimuth::BoolValue(false))
  azimuth::Attributes::set(security_attrs, "security.mfa_completed", azimuth::BoolValue(true))
  
  // 创建安全审计日志记录
  let security_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("User authentication and authorization successful"),
    Some(security_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-security-123"),
    Some("span-security-456"),
    Some(azimuth::Context::root())
  )
  
  // 验证安全日志
  assert_eq(azimuth::LogRecord::severity_number(security_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(security_log), Some("User authentication and authorization successful"))
  assert_ne(azimuth::LogRecord::attributes(security_log), None)
  
  // 发射安全日志
  azimuth::Logger::emit(logger, security_log)
  
  // 创建安全违规日志
  let violation_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(violation_attrs, "security.event_type", azimuth::StringValue("UNAUTHORIZED_ACCESS"))
  azimuth::Attributes::set(violation_attrs, "security.user_id", azimuth::StringValue("user-456"))
  azimuth::Attributes::set(violation_attrs, "security.resource", azimuth::StringValue("/api/v1/admin/delete"))
  azimuth::Attributes::set(violation_attrs, "security.action", azimuth::StringValue("DELETE"))
  azimuth::Attributes::set(violation_attrs, "security.blocked", azimuth::BoolValue(true))
  azimuth::Attributes::set(violation_attrs, "security.reason", azimuth::StringValue("Insufficient privileges"))
  azimuth::Attributes::set(violation_attrs, "security.ip_address", azimuth::StringValue("198.51.100.1"))
  azimuth::Attributes::set(violation_attrs, "security.risk_score", azimuth::FloatValue(0.9))
  
  let violation_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Security violation: Unauthorized access attempt blocked"),
    Some(violation_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-violation-789"),
    Some("span-violation-012"),
    Some(azimuth::Context::root())
  )
  
  // 验证违规日志
  assert_eq(azimuth::LogRecord::severity_number(violation_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::body(violation_log), Some("Security violation: Unauthorized access attempt blocked"))
  
  // 发射违规日志
  azimuth::Logger::emit(logger, violation_log)
}

pub test "logger with business events and metrics" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "business-events-logger")
  
  // 创建业务事件相关的属性
  let business_attrs = azimuth::Attributes::new()
  
  // 业务流程信息
  azimuth::Attributes::set(business_attrs, "business.process", azimuth::StringValue("order.fulfillment"))
  azimuth::Attributes::set(business_attrs, "business.step", azimuth::StringValue("payment.processing"))
  azimuth::Attributes::set(business_attrs, "business.transaction_id", azimuth::StringValue("txn-123456"))
  azimuth::Attributes::set(business_attrs, "business.order_id", azimuth::StringValue("order-789012"))
  azimuth::Attributes::set(business_attrs, "business.customer_id", azimuth::StringValue("cust-345678"))
  
  // 订单详情
  azimuth::Attributes::set(business_attrs, "order.amount", azimuth::FloatValue(99.99))
  azimuth::Attributes::set(business_attrs, "order.currency", azimuth::StringValue("USD"))
  azimuth::Attributes::set(business_attrs, "order.items_count", azimuth::IntValue(3))
  azimuth::Attributes::set(business_attrs, "order.product_ids", azimuth::ArrayStringValue(["prod-111", "prod-222", "prod-333"]))
  azimuth::Attributes::set(business_attrs, "order.category", azimuth::StringValue("electronics"))
  
  // 支付信息
  azimuth::Attributes::set(business_attrs, "payment.method", azimuth::StringValue("credit_card"))
  azimuth::Attributes::set(business_attrs, "payment.provider", azimuth::StringValue("stripe"))
  azimuth::Attributes::set(business_attrs, "payment.status", azimuth::StringValue("completed"))
  azimuth::Attributes::set(business_attrs, "payment.gateway_response_code", azimuth::StringValue("approved"))
  azimuth::Attributes::set(business_attrs, "payment.fee", azimuth::FloatValue(2.99))
  
  // 业务指标
  azimuth::Attributes::set(business_attrs, "metrics.revenue", azimuth::FloatValue(99.99))
  azimuth::Attributes::set(business_attrs, "metrics.profit", azimuth::FloatValue(30.00))
  azimuth::Attributes::set(business_attrs, "metrics.conversion_rate", azimuth::FloatValue(0.85))
  azimuth::Attributes::set(business_attrs, "metrics.customer_lifetime_value", azimuth::FloatValue(1250.50))
  
  // 地理位置信息
  azimuth::Attributes::set(business_attrs, "geo.country", azimuth::StringValue("US"))
  azimuth::Attributes::set(business_attrs, "geo.region", azimuth::StringValue("CA"))
  azimuth::Attributes::set(business_attrs, "geo.city", azimuth::StringValue("San Francisco"))
  
  // 创建业务事件日志记录
  let business_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Business event: Order payment completed successfully"),
    Some(business_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-business-123"),
    Some("span-business-456"),
    Some(azimuth::Context::root())
  )
  
  // 验证业务日志
  assert_eq(azimuth::LogRecord::severity_number(business_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(business_log), Some("Business event: Order payment completed successfully"))
  assert_ne(azimuth::LogRecord::attributes(business_log), None)
  
  // 发射业务日志
  azimuth::Logger::emit(logger, business_log)
}

pub test "logger with high frequency and batching" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "high-frequency-logger")
  
  // 测试高频率日志记录
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i in 0..1000 {
    let batch_attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(batch_attrs, "batch.id", azimuth::IntValue(i))
    azimuth::Attributes::set(batch_attrs, "batch.size", azimuth::IntValue(1000))
    azimuth::Attributes::set(batch_attrs, "batch.processed_at", azimuth::StringValue("2025-12-28T14:30:00Z"))
    
    let batch_log = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Processing batch item " + i.to_string()),
      Some(batch_attrs),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some("trace-batch-123"),
      Some("span-batch-456"),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(logger, batch_log)
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let duration = end_time - start_time
  
  // 验证高频率操作在合理时间内完成
  assert_true(duration < 30000000000L, "1000条日志记录应该在30秒内完成")
  
  // 测试批量日志记录
  let batch_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let large_batch_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(large_batch_attrs, "large.batch.id", azimuth::StringValue("batch-large-123"))
  azimuth::Attributes::set(large_batch_attrs, "large.batch.total_items", azimuth::IntValue(10000))
  azimuth::Attributes::set(large_batch_attrs, "large.batch.processed_items", azimuth::IntValue(10000))
  azimuth::Attributes::set(large_batch_attrs, "large.batch.success_rate", azimuth::FloatValue(1.0))
  azimuth::Attributes::set(large_batch_attrs, "large.batch.start_time", azimuth::IntValue(1735689600L))
  azimuth::Attributes::set(large_batch_attrs, "large.batch.end_time", azimuth::IntValue(1735689660L))
  azimuth::Attributes::set(large_batch_attrs, "large.batch.duration_seconds", azimuth::IntValue(60))
  
  // 添加大量项目ID
  let mut item_ids = []
  for i in 0..100 {
    item_ids.push("item-" + i.to_string())
  }
  azimuth::Attributes::set(large_batch_attrs, "large.batch.sample_items", azimuth::ArrayStringValue(item_ids))
  
  let large_batch_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Large batch processing completed"),
    Some(large_batch_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-large-batch-789"),
    Some("span-large-batch-012"),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(logger, large_batch_log)
  
  let batch_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let batch_duration = batch_end_time - batch_start_time
  
  // 验证批量日志记录在合理时间内完成
  assert_true(batch_duration < 5000000000L, "批量日志记录应该在5秒内完成")
  
  // 验证批量日志内容
  assert_eq(azimuth::LogRecord::severity_number(large_batch_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(large_batch_log), Some("Large batch processing completed"))
  assert_ne(azimuth::LogRecord::attributes(large_batch_log), None)
}

pub test "logger with multiple instances and isolation" {
  // 创建多个logger实例
  let logger_provider = azimuth::LoggerProvider::default()
  
  let auth_logger = azimuth::LoggerProvider::get_logger(logger_provider, "auth-service-logger")
  let payment_logger = azimuth::LoggerProvider::get_logger(logger_provider, "payment-service-logger")
  let notification_logger = azimuth::LoggerProvider::get_logger(logger_provider, "notification-service-logger")
  
  // 验证不同logger实例的隔离性
  assert_eq(auth_logger.scope.name, "auth-service-logger")
  assert_eq(payment_logger.scope.name, "payment-service-logger")
  assert_eq(notification_logger.scope.name, "notification-service-logger")
  
  // 认证服务日志
  let auth_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(auth_attrs, "service", azimuth::StringValue("auth"))
  azimuth::Attributes::set(auth_attrs, "operation", azimuth::StringValue("login"))
  azimuth::Attributes::set(auth_attrs, "user.id", azimuth::StringValue("user-auth-123"))
  
  let auth_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("User login successful"),
    Some(auth_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-auth-123"),
    Some("span-auth-456"),
    Some(azimuth::Context::root())
  )
  
  // 支付服务日志
  let payment_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(payment_attrs, "service", azimuth::StringValue("payment"))
  azimuth::Attributes::set(payment_attrs, "operation", azimuth::StringValue("charge"))
  azimuth::Attributes::set(payment_attrs, "transaction.id", azimuth::StringValue("txn-payment-456"))
  
  let payment_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Payment processed successfully"),
    Some(payment_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-payment-789"),
    Some("span-payment-012"),
    Some(azimuth::Context::root())
  )
  
  // 通知服务日志
  let notification_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(notification_attrs, "service", azimuth::StringValue("notification"))
  azimuth::Attributes::set(notification_attrs, "operation", azimuth::StringValue("send_email"))
  azimuth::Attributes::set(notification_attrs, "notification.id", azimuth::StringValue("notif-789"))
  
  let notification_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Email notification sent"),
    Some(notification_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-notification-345"),
    Some("span-notification-678"),
    Some(azimuth::Context::root())
  )
  
  // 发射所有日志
  azimuth::Logger::emit(auth_logger, auth_log)
  azimuth::Logger::emit(payment_logger, payment_log)
  azimuth::Logger::emit(notification_logger, notification_log)
  
  // 验证所有日志记录的完整性
  assert_eq(azimuth::LogRecord::body(auth_log), Some("User login successful"))
  assert_eq(azimuth::LogRecord::body(payment_log), Some("Payment processed successfully"))
  assert_eq(azimuth::LogRecord::body(notification_log), Some("Email notification sent"))
}