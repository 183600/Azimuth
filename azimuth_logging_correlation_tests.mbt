// Azimuth Logging and Correlation Tests
// This file contains comprehensive test cases for logging and correlation functionality

// Test 1: Basic Log Record Operations
test "basic log record operations" {
  let record = LogRecord::new(Info, "Test log message")
  
  // Test basic properties
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
  assert_eq(LogRecord::trace_id(record), None)
  assert_eq(LogRecord::span_id(record), None)
  
  // Test different severity levels
  let trace_record = LogRecord::new(Trace, "Trace message")
  let debug_record = LogRecord::new(Debug, "Debug message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  let fatal_record = LogRecord::new(Fatal, "Fatal message")
  
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
}

// Test 2: Log Record with Context and Attributes
test "log record with context and attributes" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("12345"))
  Attributes::set(attrs, "request.id", StringValue("req-67890"))
  
  let ctx = Context::root()
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace123"),
    Some("span456"),
    Some(ctx)
  )
  
  // Test all properties
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(record), Some("trace123"))
  assert_eq(LogRecord::span_id(record), Some("span456"))
  
  // Test attribute retrieval
  let user_id = Attributes::get(attrs, "user.id")
  match user_id {
    Some(StringValue(id)) => assert_eq(id, "12345")
    _ => assert_true(false)
  }
  
  let request_id = Attributes::get(attrs, "request.id")
  match request_id {
    Some(StringValue(id)) => assert_eq(id, "req-67890")
    _ => assert_true(false)
  }
}

// Test 3: Logger Operations and Emission
test "logger operations and emission" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test_logger")
  
  // Test logger properties
  let scope = Logger::instrumentation_scope(logger)
  assert_eq(scope.name, "test_logger")
  assert_eq(scope.version, None)
  assert_eq(scope.schema_url, None)
  
  // Test log emission
  let info_record = LogRecord::new(Info, "Application started")
  let error_record = LogRecord::new(Error, "Failed to connect to database")
  let debug_record = LogRecord::new(Debug, "Processing request")
  
  Logger::emit(logger, info_record)
  Logger::emit(logger, error_record)
  Logger::emit(logger, debug_record)
  
  // Test logger with version
  let versioned_logger = LoggerProvider::get_logger(provider, "versioned_logger", Some("1.2.3"))
  let versioned_scope = Logger::instrumentation_scope(versioned_logger)
  assert_eq(versioned_scope.name, "versioned_logger")
  assert_eq(versioned_scope.version, Some("1.2.3"))
}

// Test 4: Log-Trace Correlation
test "log-trace correlation" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "correlation_logger")
  
  // Create span context for correlation
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  // Create correlated log records
  let correlated_record = LogRecord::new_with_context(
    Info,
    Some("Processing user request"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  // Verify correlation
  assert_eq(LogRecord::trace_id(correlated_record), Some(trace_id))
  assert_eq(LogRecord::span_id(correlated_record), Some(span_id))
  
  // Create multiple correlated logs
  let start_record = LogRecord::new_with_context(
    Info,
    Some("Request started"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  let end_record = LogRecord::new_with_context(
    Info,
    Some("Request completed"),
    None,
    Some(1735689600000000001L),
    Some(1735689600000000001L),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  // Verify all records have same trace correlation
  assert_eq(LogRecord::trace_id(start_record), Some(trace_id))
  assert_eq(LogRecord::trace_id(correlated_record), Some(trace_id))
  assert_eq(LogRecord::trace_id(end_record), Some(trace_id))
  
  Logger::emit(logger, start_record)
  Logger::emit(logger, correlated_record)
  Logger::emit(logger, end_record)
}

// Test 5: Log Severity and Context Correlation
test "log severity and context correlation" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "severity_logger")
  
  // Create context with user information
  let ctx = Context::root()
  let user_key = ContextKey::new("user.id")
  let ctx_with_user = Context::with_value(ctx, user_key, "user123")
  
  // Create logs with different severities
  let logs = [
    (Trace, "Detailed trace information"),
    (Debug, "Debugging information"),
    (Info, "General information"),
    (Warn, "Warning condition"),
    (Error, "Error occurred"),
    (Fatal, "Fatal error")
  ]
  
  for (severity, message) in logs {
    let record = LogRecord::new_with_context(
      severity,
      Some(message),
      None,
      Some(1735689600000000000L),
      Some(1735689600000000000L),
      Some("trace123"),
      Some("span456"),
      Some(ctx_with_user)
    )
    
    assert_eq(LogRecord::severity_number(record), severity)
    assert_eq(LogRecord::body(record), Some(message))
    Logger::emit(logger, record)
  }
  
  // Verify context correlation
  let retrieved_user = Context::get(ctx_with_user, user_key)
  assert_eq(retrieved_user, Some("user123"))
}

// Test 6: Structured Logging with Attributes
test "structured logging with attributes" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "structured_logger")
  
  // Create rich attributes for structured logging
  let create_log_attributes = fn(event_type, user_id, request_id, duration) {
    let attrs = Attributes::new()
    Attributes::set(attrs, "event.type", StringValue(event_type))
    Attributes::set(attrs, "user.id", StringValue(user_id))
    Attributes::set(attrs, "request.id", StringValue(request_id))
    Attributes::set(attrs, "duration.ms", FloatValue(duration))
    Attributes::set(attrs, "service.name", StringValue("auth-service"))
    Attributes::set(attrs, "environment", StringValue("production"))
    attrs
  }
  
  // Create structured log records
  let auth_success_attrs = create_log_attributes("auth.success", "user123", "req-001", 150.0)
  let auth_failure_attrs = create_log_attributes("auth.failure", "user456", "req-002", 200.0)
  let auth_timeout_attrs = create_log_attributes("auth.timeout", "user789", "req-003", 5000.0)
  
  let success_record = LogRecord::new_with_context(
    Info,
    Some("User authentication successful"),
    Some(auth_success_attrs),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace001"),
    Some("span001"),
    Some(Context::root())
  )
  
  let failure_record = LogRecord::new_with_context(
    Warn,
    Some("User authentication failed"),
    Some(auth_failure_attrs),
    Some(1735689600000000001L),
    Some(1735689600000000001L),
    Some("trace002"),
    Some("span002"),
    Some(Context::root())
  )
  
  let timeout_record = LogRecord::new_with_context(
    Error,
    Some("User authentication timeout"),
    Some(auth_timeout_attrs),
    Some(1735689600000000002L),
    Some(1735689600000000002L),
    Some("trace003"),
    Some("span003"),
    Some(Context::root())
  )
  
  // Verify structured attributes
  let event_type = Attributes::get(auth_success_attrs, "event.type")
  match event_type {
    Some(StringValue(t)) => assert_eq(t, "auth.success")
    _ => assert_true(false)
  }
  
  let duration = Attributes::get(auth_timeout_attrs, "duration.ms")
  match duration {
    Some(FloatValue(d)) => assert_eq(d, 5000.0)
    _ => assert_true(false)
  }
  
  Logger::emit(logger, success_record)
  Logger::emit(logger, failure_record)
  Logger::emit(logger, timeout_record)
}

// Test 7: Log Aggregation and Filtering
test "log aggregation and filtering" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "aggregation_logger")
  
  // Create logs for aggregation scenarios
  let log_entries = [
    ("user.login", "user123", Info, "User logged in successfully"),
    ("user.login", "user456", Info, "User logged in successfully"),
    ("user.login", "user789", Error, "User login failed"),
    ("api.request", "/api/users", Info, "GET /api/users completed"),
    ("api.request", "/api/orders", Warn, "GET /api/orders slow response"),
    ("db.query", "SELECT * FROM users", Info, "Database query completed"),
    ("db.query", "SELECT * FROM orders", Error, "Database query failed")
  ]
  
  for (event_type, entity, severity, message) in log_entries {
    let attrs = Attributes::new()
    Attributes::set(attrs, "event.type", StringValue(event_type))
    Attributes::set(attrs, "entity", StringValue(entity))
    
    let record = LogRecord::new_with_context(
      severity,
      Some(message),
      Some(attrs),
      Some(1735689600000000000L),
      Some(1735689600000000000L),
      Some("trace123"),
      Some("span456"),
      Some(Context::root())
    )
    
    Logger::emit(logger, record)
  }
  
  // Test filtering by event type
  let filter_attrs = Attributes::new()
  Attributes::set(filter_attrs, "event.type", StringValue("user.login"))
  
  let event_type_value = Attributes::get(filter_attrs, "event.type")
  match event_type_value {
    Some(StringValue(t)) => assert_eq(t, "user.login")
    _ => assert_true(false)
  }
}

// Test 8: Cross-Service Log Correlation
test "cross-service log correlation" {
  // Service A logger
  let service_a_provider = LoggerProvider::default()
  let service_a_logger = LoggerProvider::get_logger(service_a_provider, "service-a")
  
  // Service B logger
  let service_b_provider = LoggerProvider::default()
  let service_b_logger = LoggerProvider::get_logger(service_b_provider, "service-b")
  
  // Service C logger
  let service_c_provider = LoggerProvider::default()
  let service_c_logger = LoggerProvider::get_logger(service_c_provider, "service-c")
  
  // Shared trace context for correlation
  let shared_trace_id = "cross-service-trace-123"
  let service_a_span_id = "service-a-span-456"
  let service_b_span_id = "service-b-span-789"
  let service_c_span_id = "service-c-span-012"
  
  // Create correlated logs across services
  let service_a_record = LogRecord::new_with_context(
    Info,
    Some("Service A processing request"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some(shared_trace_id),
    Some(service_a_span_id),
    Some(Context::root())
  )
  
  let service_b_record = LogRecord::new_with_context(
    Info,
    Some("Service B processing request"),
    None,
    Some(1735689600000000001L),
    Some(1735689600000000001L),
    Some(shared_trace_id),
    Some(service_b_span_id),
    Some(Context::root())
  )
  
  let service_c_record = LogRecord::new_with_context(
    Info,
    Some("Service C processing request"),
    None,
    Some(1735689600000000002L),
    Some(1735689600000000002L),
    Some(shared_trace_id),
    Some(service_c_span_id),
    Some(Context::root())
  )
  
  // Verify cross-service correlation
  assert_eq(LogRecord::trace_id(service_a_record), Some(shared_trace_id))
  assert_eq(LogRecord::trace_id(service_b_record), Some(shared_trace_id))
  assert_eq(LogRecord::trace_id(service_c_record), Some(shared_trace_id))
  
  assert_eq(LogRecord::span_id(service_a_record), Some(service_a_span_id))
  assert_eq(LogRecord::span_id(service_b_record), Some(service_b_span_id))
  assert_eq(LogRecord::span_id(service_c_record), Some(service_c_span_id))
  
  // Emit logs from different services
  Logger::emit(service_a_logger, service_a_record)
  Logger::emit(service_b_logger, service_b_record)
  Logger::emit(service_c_logger, service_c_record)
}

// Test 9: High-Frequency Logging Performance
test "high-frequency logging performance" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "performance_logger")
  
  // Test high-frequency logging
  let num_logs = 1000
  let base_timestamp = 1735689600000000000L
  
  for i in 0..num_logs {
    let timestamp = base_timestamp + i.to_int64()
    let record = LogRecord::new_with_context(
      Info,
      Some("High-frequency log entry"),
      None,
      Some(timestamp),
      Some(timestamp),
      Some("perf-trace"),
      Some("perf-span"),
      Some(Context::root())
    )
    
    Logger::emit(logger, record)
  }
  
  // Verify logger can handle high-frequency operations
  let scope = Logger::instrumentation_scope(logger)
  assert_eq(scope.name, "performance_logger")
}

// Test 10: Log Context Chain Propagation
test "log context chain propagation" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "chain_logger")
  
  // Create context chain
  let root_ctx = Context::root()
  let request_key = ContextKey::new("request.id")
  let user_key = ContextKey::new("user.id")
  let session_key = ContextKey::new("session.id")
  
  let ctx_with_request = Context::with_value(root_ctx, request_key, "req-12345")
  let ctx_with_user = Context::with_value(ctx_with_request, user_key, "user-67890")
  let ctx_with_session = Context::with_value(ctx_with_user, session_key, "sess-abcde")
  
  // Create logs with propagated context
  let start_record = LogRecord::new_with_context(
    Info,
    Some("Request started"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("chain-trace"),
    Some("chain-span"),
    Some(ctx_with_session)
  )
  
  // Verify context chain propagation
  let retrieved_request = Context::get(ctx_with_session, request_key)
  let retrieved_user = Context::get(ctx_with_session, user_key)
  let retrieved_session = Context::get(ctx_with_session, session_key)
  
  assert_eq(retrieved_request, Some("req-12345"))
  assert_eq(retrieved_user, Some("user-67890"))
  assert_eq(retrieved_session, Some("sess-abcde"))
  
  Logger::emit(logger, start_record)
}