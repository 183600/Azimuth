// 日志记录测试用例

test "logger_basic_operations" {
  // 测试日志记录器的基本操作
  
  // 创建日志提供者和记录器
  let logger_provider = azimuth::telemetry::api::logs::LoggerProvider::global()
  let logger = logger_provider.get_logger("test-logger", "1.0.0", "https://example.com/schema")
  
  // 创建日志记录
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("user.id", azimuth::telemetry::api::common::AttributeValue::String("user123"))
    .with("operation", azimuth::telemetry::api::common::AttributeValue::String("login"))
  
  let log_record = azimuth::telemetry::api::logs::LogRecord::new(
    timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    severity_number=azimuth::telemetry::api::logs::SeverityNumber::Info,
    severity_text="INFO",
    body="User login successful",
    attributes=attributes,
    trace_id=None,
    span_id=None,
    trace_flags=None
  )
  
  // 发出日志
  logger.emit(log_record)
  
  // 验证日志记录的属性
  assert_eq(log_record.severity_text, "INFO")
  assert_eq(log_record.severity_number, azimuth::telemetry::api::logs::SeverityNumber::Info)
  assert_eq(log_record.body, "User login successful")
  assert_eq(log_record.attributes.size(), 2)
  assert_eq(log_record.attributes.get("user.id").unwrap().to_string(), "user123")
  assert_eq(log_record.attributes.get("operation").unwrap().to_string(), "login")
}

test "log_severity_levels" {
  // 测试日志严重性级别
  
  let logger = azimuth::telemetry::api::logs::LoggerProvider::global().get_logger("severity-test")
  
  // 测试所有严重性级别
  let severity_levels = [
    (azimuth::telemetry::api::logs::SeverityNumber::Trace, "TRACE"),
    (azimuth::telemetry::api::logs::SeverityNumber::Debug, "DEBUG"),
    (azimuth::telemetry::api::logs::SeverityNumber::Info, "INFO"),
    (azimuth::telemetry::api::logs::SeverityNumber::Warn, "WARN"),
    (azimuth::telemetry::api::logs::SeverityNumber::Error, "ERROR"),
    (azimuth::telemetry::api::logs::SeverityNumber::Fatal, "FATAL")
  ]
  
  for (severity_number, severity_text) in severity_levels {
    let log_record = azimuth::telemetry::api::logs::LogRecord::new(
      timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
      observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
      severity_number=severity_number,
      severity_text=severity_text,
      body="Test message with " + severity_text + " severity",
      attributes=azimuth::telemetry::api::common::Attributes::empty(),
      trace_id=None,
      span_id=None,
      trace_flags=None
    )
    
    logger.emit(log_record)
    
    // 验证严重性级别
    assert_eq(log_record.severity_number, severity_number)
    assert_eq(log_record.severity_text, severity_text)
    assert_eq(log_record.body.contains(severity_text), true)
  }
}

test "log_with_trace_context" {
  // 测试带追踪上下文的日志
  
  let logger = azimuth::telemetry::api::logs::LoggerProvider::global().get_logger("trace-context-logger")
  
  // 创建追踪上下文
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_id = "00f067aa0ba902b7"
  let trace_flags = 1
  
  // 创建带追踪上下文的日志记录
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("service.name", azimuth::telemetry::api::common::AttributeValue::String("payment-service"))
    .with("operation", azimuth::telemetry::api::common::AttributeValue::String("process_payment"))
  
  let log_record = azimuth::telemetry::api::logs::LogRecord::new(
    timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    severity_number=azimuth::telemetry::api::logs::SeverityNumber::Info,
    severity_text="INFO",
    body="Payment processed successfully",
    attributes=attributes,
    trace_id=Some(trace_id),
    span_id=Some(span_id),
    trace_flags=Some(trace_flags)
  )
  
  logger.emit(log_record)
  
  // 验证追踪上下文
  assert_eq(log_record.trace_id.unwrap(), trace_id)
  assert_eq(log_record.span_id.unwrap(), span_id)
  assert_eq(log_record.trace_flags.unwrap(), trace_flags)
}

test "structured_logging_with_attributes" {
  // 测试结构化日志和属性
  
  let logger = azimuth::telemetry::api::logs::LoggerProvider::global().get_logger("structured-logger")
  
  // 创建复杂属性集
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("event.name", azimuth::telemetry::api::common::AttributeValue::String("user.registration"))
    .with("user.id", azimuth::telemetry::api::common::AttributeValue::String("user456"))
    .with("user.email", azimuth::telemetry::api::common::AttributeValue::String("user456@example.com"))
    .with("registration.source", azimuth::telemetry::api::common::AttributeValue::String("web"))
    .with("ip.address", azimuth::telemetry::api::common::AttributeValue::String("192.168.1.100"))
    .with("user.agent", azimuth::telemetry::api::common::AttributeValue::String("Mozilla/5.0..."))
    .with("timestamp", azimuth::telemetry::api::common::AttributeValue::Int64(azimuth::telemetry::sdk::platform::time::current_unix_nanos()))
    .with("session.duration", azimuth::telemetry::api::common::AttributeValue::Float64(120.5))
    .with("features.used", azimuth::telemetry::api::common::AttributeValue::ArrayString(["search", "export", "share"]))
    .with("is.premium", azimuth::telemetry::api::common::AttributeValue::Bool(false))
  
  let log_record = azimuth::telemetry::api::logs::LogRecord::new(
    timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    severity_number=azimuth::telemetry::api::logs::SeverityNumber::Info,
    severity_text="INFO",
    body="New user registration completed",
    attributes=attributes,
    trace_id=None,
    span_id=None,
    trace_flags=None
  )
  
  logger.emit(log_record)
  
  // 验证所有属性
  assert_eq(log_record.attributes.size(), 10)
  assert_eq(log_record.attributes.get("event.name").unwrap().to_string(), "user.registration")
  assert_eq(log_record.attributes.get("user.id").unwrap().to_string(), "user456")
  assert_eq(log_record.attributes.get("session.duration").unwrap().to_string(), "120.5")
  assert_eq(log_record.attributes.get("is.premium").unwrap().to_string(), "false")
  assert_eq(log_record.attributes.get("features.used").unwrap().to_string(), "[search, export, share]")
}

test "log_batch_operations" {
  // 测试日志批量操作
  
  let logger = azimuth::telemetry::api::logs::LoggerProvider::global().get_logger("batch-logger")
  
  // 创建多个日志记录
  let mut log_records = []
  
  for i = 0; i < 10; i = i + 1 {
    let attributes = azimuth::telemetry::api::common::Attributes::empty()
      .with("batch.id", azimuth::telemetry::api::common::AttributeValue::Int64(i))
      .with("operation", azimuth::telemetry::api::common::AttributeValue::String("batch.process"))
    
    let log_record = azimuth::telemetry::api::logs::LogRecord::new(
      timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
      observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
      severity_number=azimuth::telemetry::api::logs::SeverityNumber::Info,
      severity_text="INFO",
      body="Processing batch item " + i.to_string(),
      attributes=attributes,
      trace_id=None,
      span_id=None,
      trace_flags=None
    )
    
    log_records.push(log_record)
  }
  
  // 批量发出日志
  logger.emit_batch(log_records)
  
  // 验证批量日志
  assert_eq(log_records.length(), 10)
  
  for i = 0; i < log_records.length(); i = i + 1 {
    let log_record = log_records[i]
    assert_eq(log_record.body.contains("Processing batch item"), true)
    assert_eq(log_record.attributes.get("batch.id").unwrap().to_string(), i.to_string())
  }
}

test "log_error_and_exception_handling" {
  // 测试日志错误和异常处理
  
  let logger = azimuth::telemetry::api::logs::LoggerProvider::global().get_logger("error-logger")
  
  // 创建错误日志
  let error_attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("error.type", azimuth::telemetry::api::common::AttributeValue::String("DatabaseConnectionError"))
    .with("error.message", azimuth::telemetry::api::common::AttributeValue::String("Failed to connect to database"))
    .with("error.code", azimuth::telemetry::api::common::AttributeValue::Int64(500))
    .with("database.host", azimuth::telemetry::api::common::AttributeValue::String("db.example.com"))
    .with("database.port", azimuth::telemetry::api::common::AttributeValue::Int64(5432))
    .with("retry.count", azimuth::telemetry::api::common::AttributeValue::Int64(3))
  
  let error_log = azimuth::telemetry::api::logs::LogRecord::new(
    timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    severity_number=azimuth::telemetry::api::logs::SeverityNumber::Error,
    severity_text="ERROR",
    body="Database connection failed after 3 retries",
    attributes=error_attributes,
    trace_id=None,
    span_id=None,
    trace_flags=None
  )
  
  logger.emit(error_log)
  
  // 验证错误日志
  assert_eq(error_log.severity_number, azimuth::telemetry::api::logs::SeverityNumber::Error)
  assert_eq(error_log.severity_text, "ERROR")
  assert_eq(error_log.attributes.get("error.type").unwrap().to_string(), "DatabaseConnectionError")
  assert_eq(error_log.attributes.get("retry.count").unwrap().to_string(), "3")
  
  // 创建异常日志（带堆栈跟踪）
  let exception_attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("exception.type", azimuth::telemetry::api::common::AttributeValue::String("NullPointerException"))
    .with("exception.message", azimuth::telemetry::api::common::AttributeValue::String("Attempted to access null object"))
    .with("exception.stacktrace", azimuth::telemetry::api::common::AttributeValue::String("at UserService.getUser(UserService.java:45)\n at UserController.handleRequest(UserController.java:23)"))
    .with("class.name", azimuth::telemetry::api::common::AttributeValue::String("UserService"))
    .with("method.name", azimuth::telemetry::api::common::AttributeValue::String("getUser"))
    .with("line.number", azimuth::telemetry::api::common::AttributeValue::Int64(45))
  
  let exception_log = azimuth::telemetry::api::logs::LogRecord::new(
    timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    severity_number=azimuth::telemetry::api::logs::SeverityNumber::Fatal,
    severity_text="FATAL",
    body="Unhandled exception in user service",
    attributes=exception_attributes,
    trace_id=None,
    span_id=None,
    trace_flags=None
  )
  
  logger.emit(exception_log)
  
  // 验证异常日志
  assert_eq(exception_log.severity_number, azimuth::telemetry::api::logs::SeverityNumber::Fatal)
  assert_eq(exception_log.attributes.get("exception.type").unwrap().to_string(), "NullPointerException")
  assert_eq(exception_log.attributes.get("exception.stacktrace").unwrap().to_string().contains("UserService.java"), true)
}

test "log_correlation_with_tracing" {
  // 测试日志与追踪的关联
  
  // 创建追踪上下文
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b9c7c989f97918e1"
  
  // 创建带追踪上下文的记录器
  let logger = azimuth::telemetry::api::logs::LoggerProvider::global().get_logger("correlation-logger")
  
  // 模拟请求处理流程中的日志
  let request_attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("request.id", azimuth::telemetry::api::common::AttributeValue::String("req-12345"))
    .with("request.method", azimuth::telemetry::api::common::AttributeValue::String("POST"))
    .with("request.path", azimuth::telemetry::api::common::AttributeValue::String("/api/orders"))
  
  let request_log = azimuth::telemetry::api::logs::LogRecord::new(
    timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
    severity_number=azimuth::telemetry::api::logs::SeverityNumber::Info,
    severity_text="INFO",
    body="Received order creation request",
    attributes=request_attributes,
    trace_id=Some(trace_id),
    span_id=Some(span_id),
    trace_flags=Some(1)
  )
  
  // 处理过程中的日志
  let processing_attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("order.id", azimuth::telemetry::api::common::AttributeValue::String("order-67890"))
    .with("user.id", azimuth::telemetry::api::common::AttributeValue::String("user123"))
    .with("processing.step", azimuth::telemetry::api::common::AttributeValue::String("validation"))
  
  let processing_log = azimuth::telemetry::api::logs::LogRecord::new(
    timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos() + 1_000_000_000,
    observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos() + 1_000_000_000,
    severity_number=azimuth::telemetry::api::logs::SeverityNumber::Debug,
    severity_text="DEBUG",
    body="Validating order data",
    attributes=processing_attributes,
    trace_id=Some(trace_id),
    span_id=Some(span_id),
    trace_flags=Some(1)
  )
  
  // 完成处理的日志
  let completion_attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("order.id", azimuth::telemetry::api::common::AttributeValue::String("order-67890"))
    .with("processing.duration", azimuth::telemetry::api::common::AttributeValue::Float64(250.5))
    .with("status", azimuth::telemetry::api::common::AttributeValue::String("success"))
  
  let completion_log = azimuth::telemetry::api::logs::LogRecord::new(
    timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos() + 2_000_000_000,
    observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos() + 2_000_000_000,
    severity_number=azimuth::telemetry::api::logs::SeverityNumber::Info,
    severity_text="INFO",
    body="Order processing completed successfully",
    attributes=completion_attributes,
    trace_id=Some(trace_id),
    span_id=Some(span_id),
    trace_flags=Some(1)
  )
  
  // 发出所有日志
  logger.emit(request_log)
  logger.emit(processing_log)
  logger.emit(completion_log)
  
  // 验证所有日志都有相同的追踪上下文
  assert_eq(request_log.trace_id.unwrap(), trace_id)
  assert_eq(processing_log.trace_id.unwrap(), trace_id)
  assert_eq(completion_log.trace_id.unwrap(), trace_id)
  
  assert_eq(request_log.span_id.unwrap(), span_id)
  assert_eq(processing_log.span_id.unwrap(), span_id)
  assert_eq(completion_log.span_id.unwrap(), span_id)
}

test "log_performance_and_optimization" {
  // 测试日志性能和优化
  
  let logger = azimuth::telemetry::api::logs::LoggerProvider::global().get_logger("performance-logger")
  
  // 测试大量日志记录的性能
  let start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  for i = 0; i < 1000; i = i + 1 {
    let attributes = azimuth::telemetry::api::common::Attributes::empty()
      .with("iteration", azimuth::telemetry::api::common::AttributeValue::Int64(i))
      .with("operation", azimuth::telemetry::api::common::AttributeValue::String("performance.test"))
    
    let log_record = azimuth::telemetry::api::logs::LogRecord::new(
      timestamp=start_time + i.to_int64() * 1_000_000,
      observed_timestamp=start_time + i.to_int64() * 1_000_000,
      severity_number=azimuth::telemetry::api::logs::SeverityNumber::Info,
      severity_text="INFO",
      body="Performance test log entry " + i.to_string(),
      attributes=attributes,
      trace_id=None,
      span_id=None,
      trace_flags=None
    )
    
    logger.emit(log_record)
  }
  
  let end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let duration = end_time - start_time
  
  // 验证性能（应该在合理时间内完成）
  assert_eq(duration < 5_000_000_000, true)  // 小于5秒
  
  // 测试批量日志记录的性能
  let batch_start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  let mut batch_logs = []
  for i = 0; i < 1000; i = i + 1 {
    let attributes = azimuth::telemetry::api::common::Attributes::empty()
      .with("batch.iteration", azimuth::telemetry::api::common::AttributeValue::Int64(i))
    
    let log_record = azimuth::telemetry::api::logs::LogRecord::new(
      timestamp=batch_start_time + i.to_int64() * 1_000_000,
      observed_timestamp=batch_start_time + i.to_int64() * 1_000_000,
      severity_number=azimuth::telemetry::api::logs::SeverityNumber::Debug,
      severity_text="DEBUG",
      body="Batch performance test " + i.to_string(),
      attributes=attributes,
      trace_id=None,
      span_id=None,
      trace_flags=None
    )
    
    batch_logs.push(log_record)
  }
  
  logger.emit_batch(batch_logs)
  
  let batch_end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let batch_duration = batch_end_time - batch_start_time
  
  // 批量操作应该更快
  assert_eq(batch_duration < duration, true)
}

test "log_filtering_and_sampling" {
  // 测试日志过滤和采样
  
  let logger = azimuth::telemetry::api::logs::LoggerProvider::global().get_logger("filtered-logger")
  
  // 创建不同严重性级别的日志
  let log_levels = [
    (azimuth::telemetry::api::logs::SeverityNumber::Trace, "TRACE", "Detailed trace information"),
    (azimuth::telemetry::api::logs::SeverityNumber::Debug, "DEBUG", "Debug information"),
    (azimuth::telemetry::api::logs::SeverityNumber::Info, "INFO", "General information"),
    (azimuth::telemetry::api::logs::SeverityNumber::Warn, "WARN", "Warning message"),
    (azimuth::telemetry::api::logs::SeverityNumber::Error, "ERROR", "Error occurred"),
    (azimuth::telemetry::api::logs::SeverityNumber::Fatal, "FATAL", "Fatal error")
  ]
  
  // 创建带过滤器的记录器（只记录WARN及以上级别）
  let filtered_logger = logger.with_min_severity(azimuth::telemetry::api::logs::SeverityNumber::Warn)
  
  for (severity_number, severity_text, message) in log_levels {
    let log_record = azimuth::telemetry::api::logs::LogRecord::new(
      timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
      observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
      severity_number=severity_number,
      severity_text=severity_text,
      body=message,
      attributes=azimuth::telemetry::api::common::Attributes::empty(),
      trace_id=None,
      span_id=None,
      trace_flags=None
    )
    
    filtered_logger.emit(log_record)
  }
  
  // 创建采样记录器（采样率50%）
  let sampled_logger = logger.with_sampling_rate(0.5)
  
  for i = 0; i < 100; i = i + 1 {
    let log_record = azimuth::telemetry::api::logs::LogRecord::new(
      timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
      observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
      severity_number=azimuth::telemetry::api::logs::SeverityNumber::Info,
      severity_text="INFO",
      body="Sampled log entry " + i.to_string(),
      attributes=azimuth::telemetry::api::common::Attributes::empty(),
      trace_id=None,
      span_id=None,
      trace_flags=None
    )
    
    sampled_logger.emit(log_record)
  }
  
  // 验证过滤器和采样器配置
  assert_eq(filtered_logger.min_severity(), azimuth::telemetry::api::logs::SeverityNumber::Warn)
  assert_eq(sampled_logger.sampling_rate(), 0.5)
}