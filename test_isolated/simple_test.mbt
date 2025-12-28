// Simple test to verify basic functionality
test "basic_span_creation" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test_tracer")
  let span = Tracer::start_span(tracer, "test_span")
  
  @assertion.assert_eq(Span::name(span), "test_span")!
}

test "basic_counter_operations" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test_meter")
  let counter = Meter::create_counter(meter, "test_counter")
  
  Counter::add(counter, 5.0)
  // Test passes if no errors occur
}

test "basic_log_record" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test_logger")
  let log = LogRecord::new(Info, "Test log message")
  
  @assertion.assert_eq(LogRecord::severity_number(log), Info)!
  @assertion.assert_eq(LogRecord::body(log), Some("Test log message"))!
}