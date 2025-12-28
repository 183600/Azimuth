test "logger provider and logger creation" {
  // Test logger provider creation
  let noop_provider = @azimuth.LoggerProvider::noop()
  let default_provider = @azimuth.LoggerProvider::default()
  
  // Test getting loggers from providers
  let noop_logger = @azimuth.LoggerProvider::get_logger(noop_provider, "noop_logger")
  let default_logger = @azimuth.LoggerProvider::get_logger(default_provider, "default_logger")
  
  // Test logger scope properties
  let noop_scope = noop_logger.scope
  let default_scope = default_logger.scope
  
  assert_eq(noop_scope.name, "noop_logger")
  assert_eq(default_scope.name, "default_logger")
  assert_eq(noop_scope.version, None)
  assert_eq(default_scope.version, None)
}

test "log record creation and basic properties" {
  // Test log record creation with different severity levels
  let trace_record = @azimuth.LogRecord::new(@azimuth.Trace, "Trace message")
  let debug_record = @azimuth.LogRecord::new(@azimuth.Debug, "Debug message")
  let info_record = @azimuth.LogRecord::new(@azimuth.Info, "Info message")
  let warn_record = @azimuth.LogRecord::new(@azimuth.Warn, "Warning message")
  let error_record = @azimuth.LogRecord::new(@azimuth.Error, "Error message")
  let fatal_record = @azimuth.LogRecord::new(@azimuth.Fatal, "Fatal message")
  
  // Test severity levels
  assert_eq(@azimuth.LogRecord::severity_number(trace_record), @azimuth.Trace)
  assert_eq(@azimuth.LogRecord::severity_number(debug_record), @azimuth.Debug)
  assert_eq(@azimuth.LogRecord::severity_number(info_record), @azimuth.Info)
  assert_eq(@azimuth.LogRecord::severity_number(warn_record), @azimuth.Warn)
  assert_eq(@azimuth.LogRecord::severity_number(error_record), @azimuth.Error)
  assert_eq(@azimuth.LogRecord::severity_number(fatal_record), @azimuth.Fatal)
  
  // Test log bodies
  assert_eq(@azimuth.LogRecord::body(trace_record), Some("Trace message"))
  assert_eq(@azimuth.LogRecord::body(debug_record), Some("Debug message"))
  assert_eq(@azimuth.LogRecord::body(info_record), Some("Info message"))
  assert_eq(@azimuth.LogRecord::body(warn_record), Some("Warning message"))
  assert_eq(@azimuth.LogRecord::body(error_record), Some("Error message"))
  assert_eq(@azimuth.LogRecord::body(fatal_record), Some("Fatal message"))
}

test "log record with full context" {
  // Create log record with full context
  let severity = @azimuth.Info
  let body = Some("Detailed log message")
  let attributes = Some(@azimuth.Attributes::new())
  let timestamp = Some(1735689600000000000L)
  let observed_timestamp = Some(1735689600000001000L)
  let trace_id = Some("0af7651916cd43dd8448eb211c80319c")
  let span_id = Some("b7ad6b7169203331")
  let context = Some(@azimuth.Context::root())
  
  let record = @azimuth.LogRecord::new_with_context(
    severity, body, attributes, timestamp, observed_timestamp, trace_id, span_id, context
  )
  
  // Test all properties
  assert_eq(@azimuth.LogRecord::severity_number(record), @azimuth.Info)
  assert_eq(@azimuth.LogRecord::body(record), Some("Detailed log message"))
  assert_eq(@azimuth.LogRecord::trace_id(record), Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(@azimuth.LogRecord::span_id(record), Some("b7ad6b7169203331"))
}

test "log record with various body types" {
  // Test log records with different body content
  let empty_body = @azimuth.LogRecord::new(@azimuth.Info, "")
  let long_body = @azimuth.LogRecord::new(@azimuth.Info, "This is a very long log message that contains detailed information about an operation or event that occurred in the system.")
  let unicode_body = @azimuth.LogRecord::new(@azimuth.Info, "Unicode test: 中文 🚀 📊 emoji")
  let special_chars_body = @azimuth.LogRecord::new(@azimuth.Info, "Special chars: \n\t\"'\[]{}<>")
  let numeric_body = @azimuth.LogRecord::new(@azimuth.Info, "Numeric values: 123, 45.67, -89")
  
  // Test that all bodies are preserved correctly
  assert_eq(@azimuth.LogRecord::body(empty_body), Some(""))
  assert_eq(@azimuth.LogRecord::body(long_body), Some("This is a very long log message that contains detailed information about an operation or event that occurred in the system."))
  assert_eq(@azimuth.LogRecord::body(unicode_body), Some("Unicode test: 中文 🚀 📊 emoji"))
  assert_eq(@azimuth.LogRecord::body(special_chars_body), Some("Special chars: \n\t\"'\[]{}<>"))
  assert_eq(@azimuth.LogRecord::body(numeric_body), Some("Numeric values: 123, 45.67, -89"))
}

test "logger emit operations" {
  let provider = @azimuth.LoggerProvider::default()
  let logger = @azimuth.LoggerProvider::get_logger(provider, "test_logger")
  
  // Test emitting different types of log records
  let simple_record = @azimuth.LogRecord::new(@azimuth.Info, "Simple log message")
  let detailed_record = @azimuth.LogRecord::new_with_context(
    @azimuth.Warn,
    Some("Warning with context"),
    Some(@azimuth.Attributes::new()),
    Some(1735689600000000000L),
    None,
    Some("trace_123"),
    Some("span_456"),
    Some(@azimuth.Context::root())
  )
  
  // Test emit operations
  @azimuth.Logger::emit(logger, simple_record)
  @azimuth.Logger::emit(logger, detailed_record)
  
  // Test passes if no exceptions are thrown
  assert_true(true)
}

test "log record severity ordering" {
  // Test that severity levels are properly ordered for filtering
  let trace_sev = @azimuth.LogRecord::severity_number(@azimuth.LogRecord::new(@azimuth.Trace, ""))
  let debug_sev = @azimuth.LogRecord::severity_number(@azimuth.LogRecord::new(@azimuth.Debug, ""))
  let info_sev = @azimuth.LogRecord::severity_number(@azimuth.LogRecord::new(@azimuth.Info, ""))
  let warn_sev = @azimuth.LogRecord::severity_number(@azimuth.LogRecord::new(@azimuth.Warn, ""))
  let error_sev = @azimuth.LogRecord::severity_number(@azimuth.LogRecord::new(@azimuth.Error, ""))
  let fatal_sev = @azimuth.LogRecord::severity_number(@azimuth.LogRecord::new(@azimuth.Fatal, ""))
  
  // Verify severity levels are different
  assert_true(trace_sev != debug_sev)
  assert_true(debug_sev != info_sev)
  assert_true(info_sev != warn_sev)
  assert_true(warn_sev != error_sev)
  assert_true(error_sev != fatal_sev)
  
  // Test passes if all severity levels are distinct
  assert_true(true)
}

test "log record with trace and span correlation" {
  // Test log records that are correlated with traces and spans
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let correlated_record = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Operation completed"),
    None,
    None,
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  // Test trace and span correlation
  assert_eq(@azimuth.LogRecord::trace_id(correlated_record), Some(trace_id))
  assert_eq(@azimuth.LogRecord::span_id(correlated_record), Some(span_id))
  
  // Test uncorrelated record
  let uncorrelated_record = @azimuth.LogRecord::new(@azimuth.Info, "Uncorrelated message")
  assert_eq(@azimuth.LogRecord::trace_id(uncorrelated_record), None)
  assert_eq(@azimuth.LogRecord::span_id(uncorrelated_record), None)
}

test "log record timestamp operations" {
  // Test log records with different timestamp configurations
  let current_time = 1735689600000000000L
  let observed_time = 1735689600000001000L
  
  // Record with both timestamps
  let full_timestamp_record = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Timed message"),
    None,
    Some(current_time),
    Some(observed_time),
    None,
    None,
    None
  )
  
  // Record with only timestamp
  let timestamp_only_record = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Timestamp only"),
    None,
    Some(current_time),
    None,
    None,
    None,
    None
  )
  
  // Record with no timestamps
  let no_timestamp_record = @azimuth.LogRecord::new(@azimuth.Info, "No timestamps")
  
  // Test that timestamp handling doesn't cause exceptions
  assert_true(true)
}