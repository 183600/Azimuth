// Simple Practical Telemetry Tests for Azimuth
// This file contains basic test cases to verify functionality

test "basic span context validation" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
}

test "basic metrics operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "http.requests.total")
  
  assert_eq(counter.name, "http.requests.total")
  Counter::add(counter, 1.0)
  assert_true(true)
}

test "basic context operations" {
  let ctx = Context::root()
  let key = ContextKey::new("test.key")
  let ctx_with_value = Context::with_value(ctx, key, "test_value")
  
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test_value"))
}

test "basic logging operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test.logger")
  
  let record = LogRecord::new(Info, "Test log message")
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
  
  Logger::emit(logger, record)
  assert_true(true)
}

test "telemetry end-to-end workflow" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "workflow.test", Some("1.0.0"))
  
  let span = Tracer::start_span(tracer, "user.request.processing")
  assert_eq(Span::name(span), "user.request.processing")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  Span::set_status(span, Ok, Some("Request processed successfully"))
  Span::add_event(span, "validation.completed", Some([("user.id", StringValue("12345"))]))
  Span::end(span)
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "workflow.metrics")
  let counter = Meter::create_counter(meter, "requests.processed")
  Counter::add(counter, 1.0)
  
  assert_true(true)
}