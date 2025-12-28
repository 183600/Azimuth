// Enhanced Log Record Severity Level Tests for Azimuth
// Tests various log severity levels and log record management

test "log record severity level transitions" {
  // Test creating log records with all severity levels
  let trace_record = LogRecord::new(Trace, "Trace level message")
  let debug_record = LogRecord::new(Debug, "Debug level message")
  let info_record = LogRecord::new(Info, "Info level message")
  let warn_record = LogRecord::new(Warn, "Warning level message")
  let error_record = LogRecord::new(Error, "Error level message")
  let fatal_record = LogRecord::new(Fatal, "Fatal level message")
  
  // Verify severity levels
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  // Verify log bodies
  assert_eq(LogRecord::body(trace_record), Some("Trace level message"))
  assert_eq(LogRecord::body(debug_record), Some("Debug level message"))
  assert_eq(LogRecord::body(info_record), Some("Info level message"))
  assert_eq(LogRecord::body(warn_record), Some("Warning level message"))
  assert_eq(LogRecord::body(error_record), Some("Error level message"))
  assert_eq(LogRecord::body(fatal_record), Some("Fatal level message"))
}

test "log record with context and severity progression" {
  let ctx = Context::root()
  let user_key = ContextKey::new("user.id")
  let request_key = ContextKey::new("request.id")
  let session_key = ContextKey::new("session.id")
  
  let ctx_with_user = Context::with_value(ctx, user_key, "user-12345")
  let ctx_with_request = Context::with_value(ctx_with_user, request_key, "req-67890")
  let ctx_with_session = Context::with_value(ctx_with_request, session_key, "session-abcde")
  
  // Create log records with severity progression (Trace -> Debug -> Info -> Warn -> Error -> Fatal)
  let trace_log = LogRecord::new_with_context(
    Trace, Some("Starting operation"), None, Some(1000L), Some(1001L), 
    Some("trace-123"), Some("span-123"), Some(ctx_with_session)
  )
  
  let debug_log = LogRecord::new_with_context(
    Debug, Some("Processing data"), None, Some(1002L), Some(1003L),
    Some("trace-123"), Some("span-123"), Some(ctx_with_session)
  )
  
  let info_log = LogRecord::new_with_context(
    Info, Some("Operation in progress"), None, Some(1004L), Some(1005L),
    Some("trace-123"), Some("span-123"), Some(ctx_with_session)
  )
  
  let warn_log = LogRecord::new_with_context(
    Warn, Some("Operation slower than expected"), None, Some(1006L), Some(1007L),
    Some("trace-123"), Some("span-123"), Some(ctx_with_session)
  )
  
  let error_log = LogRecord::new_with_context(
    Error, Some("Operation failed"), None, Some(1008L), Some(1009L),
    Some("trace-123"), Some("span-123"), Some(ctx_with_session)
  )
  
  let fatal_log = LogRecord::new_with_context(
    Fatal, Some("System crash"), None, Some(1010L), Some(1011L),
    Some("trace-123"), Some("span-123"), Some(ctx_with_session)
  )
  
  // Verify all logs have correct severity and context
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  assert_eq(LogRecord::trace_id(trace_log), Some("trace-123"))
  assert_eq(LogRecord::span_id(trace_log), Some("span-123"))
  assert_eq(LogRecord::trace_id(fatal_log), Some("trace-123"))
  assert_eq(LogRecord::span_id(fatal_log), Some("span-123"))
}

test "log record with empty and null bodies" {
  // Test log records with empty bodies
  let empty_trace = LogRecord::new(Trace, "")
  let empty_debug = LogRecord::new(Debug, "")
  let empty_info = LogRecord::new(Info, "")
  let empty_warn = LogRecord::new(Warn, "")
  let empty_error = LogRecord::new(Error, "")
  let empty_fatal = LogRecord::new(Fatal, "")
  
  assert_eq(LogRecord::body(empty_trace), Some(""))
  assert_eq(LogRecord::body(empty_debug), Some(""))
  assert_eq(LogRecord::body(empty_info), Some(""))
  assert_eq(LogRecord::body(empty_warn), Some(""))
  assert_eq(LogRecord::body(empty_error), Some(""))
  assert_eq(LogRecord::body(empty_fatal), Some(""))
  
  // Test log records with None bodies (using new_with_context)
  let none_body_trace = LogRecord::new_with_context(Trace, None, None, None, None, None, None, None)
  let none_body_debug = LogRecord::new_with_context(Debug, None, None, None, None, None, None, None)
  let none_body_info = LogRecord::new_with_context(Info, None, None, None, None, None, None, None)
  let none_body_warn = LogRecord::new_with_context(Warn, None, None, None, None, None, None, None)
  let none_body_error = LogRecord::new_with_context(Error, None, None, None, None, None, None, None)
  let none_body_fatal = LogRecord::new_with_context(Fatal, None, None, None, None, None, None, None)
  
  assert_eq(LogRecord::body(none_body_trace), None)
  assert_eq(LogRecord::body(none_body_debug), None)
  assert_eq(LogRecord::body(none_body_info), None)
  assert_eq(LogRecord::body(none_body_warn), None)
  assert_eq(LogRecord::body(none_body_error), None)
  assert_eq(LogRecord::body(none_body_fatal), None)
}

test "log record with special character bodies" {
  // Test log records with special characters at different severity levels
  let special_chars_trace = LogRecord::new(Trace, "Special chars: !@#$%^&*()")
  let special_chars_debug = LogRecord::new(Debug, "Newlines:\nLine1\nLine2\nLine3")
  let special_chars_info = LogRecord::new(Info, "Tabs:\tTab1\tTab2\tTab3")
  let special_chars_warn = LogRecord::new(Warn, "Quotes: \"Single\" and 'Double'")
  let special_chars_error = LogRecord::new(Error, "Unicode: 测试 🚀 ñáéíóú")
  let special_chars_fatal = LogRecord::new(Fatal, "JSON: {\"key\":\"value\",\"array\":[1,2,3]}")
  
  assert_eq(LogRecord::severity_number(special_chars_trace), Trace)
  assert_eq(LogRecord::severity_number(special_chars_debug), Debug)
  assert_eq(LogRecord::severity_number(special_chars_info), Info)
  assert_eq(LogRecord::severity_number(special_chars_warn), Warn)
  assert_eq(LogRecord::severity_number(special_chars_error), Error)
  assert_eq(LogRecord::severity_number(special_chars_fatal), Fatal)
  
  assert_eq(LogRecord::body(special_chars_trace), Some("Special chars: !@#$%^&*()"))
  assert_eq(LogRecord::body(special_chars_debug), Some("Newlines:\nLine1\nLine2\nLine3"))
  assert_eq(LogRecord::body(special_chars_info), Some("Tabs:\tTab1\tTab2\tTab3"))
  assert_eq(LogRecord::body(special_chars_warn), Some("Quotes: \"Single\" and 'Double'"))
  assert_eq(LogRecord::body(special_chars_error), Some("Unicode: 测试 🚀 ñáéíóú"))
  assert_eq(LogRecord::body(special_chars_fatal), Some("JSON: {\"key\":\"value\",\"array\":[1,2,3]}"))
}

test "log record with very long bodies" {
  // Create very long log messages for each severity level
  let long_message = "This is a very long log message that exceeds normal length expectations. ".repeat(100)
  
  let long_trace = LogRecord::new(Trace, long_message + " [TRACE]")
  let long_debug = LogRecord::new(Debug, long_message + " [DEBUG]")
  let long_info = LogRecord::new(Info, long_message + " [INFO]")
  let long_warn = LogRecord::new(Warn, long_message + " [WARN]")
  let long_error = LogRecord::new(Error, long_message + " [ERROR]")
  let long_fatal = LogRecord::new(Fatal, long_message + " [FATAL]")
  
  // Verify severity levels are maintained
  assert_eq(LogRecord::severity_number(long_trace), Trace)
  assert_eq(LogRecord::severity_number(long_debug), Debug)
  assert_eq(LogRecord::severity_number(long_info), Info)
  assert_eq(LogRecord::severity_number(long_warn), Warn)
  assert_eq(LogRecord::severity_number(long_error), Error)
  assert_eq(LogRecord::severity_number(long_fatal), Fatal)
  
  // Verify long bodies are preserved
  assert_eq(LogRecord::body(long_trace), Some(long_message + " [TRACE]"))
  assert_eq(LogRecord::body(long_debug), Some(long_message + " [DEBUG]"))
  assert_eq(LogRecord::body(long_info), Some(long_message + " [INFO]"))
  assert_eq(LogRecord::body(long_warn), Some(long_message + " [WARN]"))
  assert_eq(LogRecord::body(long_error), Some(long_message + " [ERROR]"))
  assert_eq(LogRecord::body(long_fatal), Some(long_message + " [FATAL]"))
}

test "log record timestamp handling with severity" {
  let base_timestamp = 1609459200000000000L  // 2021-01-01 00:00:00 UTC
  
  // Create log records with sequential timestamps for each severity
  let trace_with_time = LogRecord::new_with_context(
    Trace, Some("Trace with timestamp"), None, Some(base_timestamp), Some(base_timestamp + 1L), None, None, None
  )
  
  let debug_with_time = LogRecord::new_with_context(
    Debug, Some("Debug with timestamp"), None, Some(base_timestamp + 1000L), Some(base_timestamp + 1001L), None, None, None
  )
  
  let info_with_time = LogRecord::new_with_context(
    Info, Some("Info with timestamp"), None, Some(base_timestamp + 2000L), Some(base_timestamp + 2001L), None, None, None
  )
  
  let warn_with_time = LogRecord::new_with_context(
    Warn, Some("Warning with timestamp"), None, Some(base_timestamp + 3000L), Some(base_timestamp + 3001L), None, None, None
  )
  
  let error_with_time = LogRecord::new_with_context(
    Error, Some("Error with timestamp"), None, Some(base_timestamp + 4000L), Some(base_timestamp + 4001L), None, None, None
  )
  
  let fatal_with_time = LogRecord::new_with_context(
    Fatal, Some("Fatal with timestamp"), None, Some(base_timestamp + 5000L), Some(base_timestamp + 5001L), None, None, None
  )
  
  // Verify severity levels and timestamps
  assert_eq(LogRecord::severity_number(trace_with_time), Trace)
  assert_eq(LogRecord::severity_number(debug_with_time), Debug)
  assert_eq(LogRecord::severity_number(info_with_time), Info)
  assert_eq(LogRecord::severity_number(warn_with_time), Warn)
  assert_eq(LogRecord::severity_number(error_with_time), Error)
  assert_eq(LogRecord::severity_number(fatal_with_time), Fatal)
  
  // Note: Simplified implementation doesn't expose timestamp accessors
  // This test documents expected behavior for timestamp handling
}

test "logger emission with different severity levels" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "severity-test-logger")
  
  // Create log records for each severity level
  let trace_record = LogRecord::new(Trace, "Trace level log")
  let debug_record = LogRecord::new(Debug, "Debug level log")
  let info_record = LogRecord::new(Info, "Info level log")
  let warn_record = LogRecord::new(Warn, "Warning level log")
  let error_record = LogRecord::new(Error, "Error level log")
  let fatal_record = LogRecord::new(Fatal, "Fatal level log")
  
  // Emit all log records
  Logger::emit(logger, trace_record)
  Logger::emit(logger, debug_record)
  Logger::emit(logger, info_record)
  Logger::emit(logger, warn_record)
  Logger::emit(logger, error_record)
  Logger::emit(logger, fatal_record)
  
  // Verify logger properties
  assert_eq(logger.scope.name, "severity-test-logger")
  
  // Verify log records are still valid after emission
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
}

test "log record severity comparison and filtering" {
  // Create log records with different severities
  let logs = [
    LogRecord::new(Trace, "Trace message"),
    LogRecord::new(Debug, "Debug message"),
    LogRecord::new(Info, "Info message"),
    LogRecord::new(Warn, "Warning message"),
    LogRecord::new(Error, "Error message"),
    LogRecord::new(Fatal, "Fatal message")
  ]
  
  // Test severity level comparisons (conceptual)
  // In a full implementation, this would include severity comparison functions
  assert_eq(LogRecord::severity_number(logs[0]), Trace)
  assert_eq(LogRecord::severity_number(logs[1]), Debug)
  assert_eq(LogRecord::severity_number(logs[2]), Info)
  assert_eq(LogRecord::severity_number(logs[3]), Warn)
  assert_eq(LogRecord::severity_number(logs[4]), Error)
  assert_eq(LogRecord::severity_number(logs[5]), Fatal)
  
  // Test filtering conceptually (would be implemented with severity comparison)
  // Filter logs with severity >= Info
  let info_and_above = [logs[2], logs[3], logs[4], logs[5]]  // Info, Warn, Error, Fatal
  let warn_and_above = [logs[3], logs[4], logs[5]]           // Warn, Error, Fatal
  let error_and_above = [logs[4], logs[5]]                   // Error, Fatal
  
  // Verify filtered sets have correct severities
  assert_eq(LogRecord::severity_number(info_and_above[0]), Info)
  assert_eq(LogRecord::severity_number(info_and_above[1]), Warn)
  assert_eq(LogRecord::severity_number(info_and_above[2]), Error)
  assert_eq(LogRecord::severity_number(info_and_above[3]), Fatal)
  
  assert_eq(LogRecord::severity_number(warn_and_above[0]), Warn)
  assert_eq(LogRecord::severity_number(warn_and_above[1]), Error)
  assert_eq(LogRecord::severity_number(warn_and_above[2]), Fatal)
  
  assert_eq(LogRecord::severity_number(error_and_above[0]), Error)
  assert_eq(LogRecord::severity_number(error_and_above[1]), Fatal)
}

test "log record with mixed severity workflow" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "workflow-logger")
  
  let ctx = Context::root()
  let correlation_key = ContextKey::new("correlation.id")
  let ctx_with_correlation = Context::with_value(ctx, correlation_key, "corr-12345")
  
  // Simulate a complex workflow with different severity levels
  let trace_id = "workflow-trace-123"
  let span_id = "workflow-span-456"
  
  // 1. Start workflow (Trace)
  let start_log = LogRecord::new_with_context(
    Trace, Some("Starting workflow execution"), None, Some(1000L), Some(1001L),
    Some(trace_id), Some(span_id), Some(ctx_with_correlation)
  )
  Logger::emit(logger, start_log)
  
  // 2. Initialize components (Debug)
  let init_log = LogRecord::new_with_context(
    Debug, Some("Initializing workflow components"), None, Some(1002L), Some(1003L),
    Some(trace_id), Some(span_id), Some(ctx_with_correlation)
  )
  Logger::emit(logger, init_log)
  
  // 3. Process data (Info)
  let process_log = LogRecord::new_with_context(
    Info, Some("Processing workflow data"), None, Some(1004L), Some(1005L),
    Some(trace_id), Some(span_id), Some(ctx_with_correlation)
  )
  Logger::emit(logger, process_log)
  
  // 4. Performance warning (Warn)
  let perf_log = LogRecord::new_with_context(
    Warn, Some("Workflow taking longer than expected"), None, Some(1006L), Some(1007L),
    Some(trace_id), Some(span_id), Some(ctx_with_correlation)
  )
  Logger::emit(logger, perf_log)
  
  // 5. Handle error (Error)
  let error_log = LogRecord::new_with_context(
    Error, Some("Error in workflow processing"), None, Some(1008L), Some(1009L),
    Some(trace_id), Some(span_id), Some(ctx_with_correlation)
  )
  Logger::emit(logger, error_log)
  
  // 6. Recovery attempt (Info)
  let recovery_log = LogRecord::new_with_context(
    Info, Some("Attempting workflow recovery"), None, Some(1010L), Some(1011L),
    Some(trace_id), Some(span_id), Some(ctx_with_correlation)
  )
  Logger::emit(logger, recovery_log)
  
  // 7. Successful completion (Info)
  let success_log = LogRecord::new_with_context(
    Info, Some("Workflow completed successfully"), None, Some(1012L), Some(1013L),
    Some(trace_id), Some(span_id), Some(ctx_with_correlation)
  )
  Logger::emit(logger, success_log)
  
  // Verify all log records have correct properties
  assert_eq(LogRecord::severity_number(start_log), Trace)
  assert_eq(LogRecord::severity_number(init_log), Debug)
  assert_eq(LogRecord::severity_number(process_log), Info)
  assert_eq(LogRecord::severity_number(perf_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(recovery_log), Info)
  assert_eq(LogRecord::severity_number(success_log), Info)
  
  // Verify trace and span consistency
  assert_eq(LogRecord::trace_id(start_log), Some(trace_id))
  assert_eq(LogRecord::span_id(start_log), Some(span_id))
  assert_eq(LogRecord::trace_id(success_log), Some(trace_id))
  assert_eq(LogRecord::span_id(success_log), Some(span_id))
}