// Advanced Test Suite for Azimuth Telemetry System
// Essential complex scenarios and edge cases

test "tracer and span creation workflow" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer", Some("1.0.0"))
  
  // Test tracer properties
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test-tracer")
  assert_eq(scope.version, Some("1.0.0"))
  
  // Test span creation with attributes
  let span_attrs = [("http.method", StringValue("GET")), ("http.url", StringValue("/api/users"))]
  let span = Tracer::start_span(tracer, "http-request", Some(span_attrs))
  
  // Test span properties
  assert_eq(Span::name(span), "http-request")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  // Test span context
  let span_ctx = Span::span_context(span)
  assert_true(SpanContext::is_valid(span_ctx))
  assert_true(SpanContext::is_sampled(span_ctx))
}

test "logger provider and record emission" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  // Test logger scope
  let scope = logger.scope
  assert_eq(scope.name, "test-logger")
  
  // Test creating and emitting different severity log records
  let debug_record = LogRecord::new(Debug, "Debug message")
  let info_record = LogRecord::new(Info, "Info message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  
  // Test severity levels
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  
  // Test emitting log records
  Logger::emit(logger, debug_record)
  Logger::emit(logger, info_record)
  Logger::emit(logger, warn_record)
  Logger::emit(logger, error_record)
  
  assert_true(true) // If we reach here, emission didn't crash
}