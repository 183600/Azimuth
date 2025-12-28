// Test verification with main function
fn main {
  test_basic_span_creation()
  test_basic_counter_operations()
  test_basic_log_record()
  println("All tests passed!")
}

fn test_basic_span_creation() -> Unit {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test_tracer")
  let span = Tracer::start_span(tracer, "test_span")
  
  if Span::name(span) != "test_span" {
    panic("Span name test failed")
  }
}

fn test_basic_counter_operations() -> Unit {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test_meter")
  let counter = Meter::create_counter(meter, "test_counter")
  
  Counter::add(counter, 5.0)
  // Test passes if no errors occur
}

fn test_basic_log_record() -> Unit {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test_logger")
  let log = LogRecord::new(Info, "Test log message")
  
  if LogRecord::severity_number(log) != Info {
    panic("Log severity test failed")
  }
  
  if LogRecord::body(log) != Some("Test log message") {
    panic("Log body test failed")
  }
}