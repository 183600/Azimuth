// LogRecord Test Cases for Azimuth Telemetry System
// Testing log record creation, severity levels, and context integration

test "log record creation and basic properties" {
  let record = LogRecord::new(Info, "Test log message")
  
  // Test basic properties
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
  
  // Test different severity levels
  let trace_record = LogRecord::new(Trace, "Trace message")
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  
  let debug_record = LogRecord::new(Debug, "Debug message")
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  
  let warn_record = LogRecord::new(Warn, "Warning message")
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  
  let error_record = LogRecord::new(Error, "Error message")
  assert_eq(LogRecord::severity_number(error_record), Error)
  
  let fatal_record = LogRecord::new(Fatal, "Fatal message")
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
}

test "log record with full context" {
  let attrs = Attributes::new()
  let context = Context::root()
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-123"),
    Some("span-456"),
    Some(context)
  )
  
  // Test all properties
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(record), Some("trace-123"))
  assert_eq(LogRecord::span_id(record), Some("span-456"))
}

test "log record with attributes" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "error.code", IntValue(500))
  Attributes::set(attrs, "error.type", StringValue("DatabaseError"))
  Attributes::set(attrs, "retry.count", IntValue(3))
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Operation failed after retries"),
    Some(attrs),
    Some(1735689600000000000L),
    None,
    None,
    None,
    None
  )
  
  // Test properties
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Operation failed after retries"))
}

test "logger provider and logger operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  // Test logger properties
  let scope = logger.scope
  assert_eq(scope.name, "test-logger")
  assert_eq(scope.version, None)
  assert_eq(scope.schema_url, None)
  
  // Test creating and emitting log records
  let info_record = LogRecord::new(Info, "Application started")
  let warn_record = LogRecord::new(Warn, "Configuration missing, using defaults")
  let error_record = LogRecord::new(Error, "Service unavailable")
  
  // Emit log records
  Logger::emit(logger, info_record)
  Logger::emit(logger, warn_record)
  Logger::emit(logger, error_record)
  
  // Verify operations complete without errors
  assert_true(true)
}

test "log record severity hierarchy" {
  // Test severity level ordering (lower values = less severe)
  assert_true(Trace as Int < Debug as Int)
  assert_true(Debug as Int < Info as Int)
  assert_true(Info as Int < Warn as Int)
  assert_true(Warn as Int < Error as Int)
  assert_true(Error as Int < Fatal as Int)
  
  // Test severity comparisons
  let trace_record = LogRecord::new(Trace, "Trace level")
  let info_record = LogRecord::new(Info, "Info level")
  let error_record = LogRecord::new(Error, "Error level")
  
  assert_true(LogRecord::severity_number(trace_record) as Int < LogRecord::severity_number(info_record) as Int)
  assert_true(LogRecord::severity_number(info_record) as Int < LogRecord::severity_number(error_record) as Int)
}

test "log record with timestamps" {
  let base_timestamp = 1735689600000000000L
  
  // Test log record with timestamp
  let record_with_timestamp = LogRecord::new_with_context(
    Info,
    Some("Timestamped log"),
    None,
    Some(base_timestamp),
    Some(base_timestamp + 1000L),
    None,
    None,
    None
  )
  
  // Test log record without timestamp
  let record_without_timestamp = LogRecord::new(Info, "Non-timestamped log")
  
  // Verify both records are created successfully
  assert_eq(LogRecord::body(record_with_timestamp), Some("Timestamped log"))
  assert_eq(LogRecord::body(record_without_timestamp), Some("Non-timestamped log"))
}

test "log record with trace and span correlation" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let record = LogRecord::new_with_context(
    Warn,
    Some("Operation completed with warnings"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  // Test trace and span correlation
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
  assert_eq(LogRecord::severity_number(record), Warn)
}