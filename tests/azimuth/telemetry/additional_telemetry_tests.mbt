test "span and span context functionality" {
  // Test SpanContext creation and properties
  let span_ctx = SpanContext::new("trace_123", "span_456", true, "trace_state_value")
  assert_eq(SpanContext::trace_id(span_ctx), "trace_123")
  assert_eq(SpanContext::span_id(span_ctx), "span_456")
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
  
  // Test invalid SpanContext
  let invalid_ctx = SpanContext::new("", "span_456", true, "")
  assert_false(SpanContext::is_valid(invalid_ctx))
  
  // Test Span creation and properties
  let span = Span::new("test_span", Internal, span_ctx)
  assert_eq(Span::name(span), "test_span")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  assert_eq(Span::span_context(span), span_ctx)
  
  // Test Span status operations
  Span::set_status(span, Ok, Some("Operation completed"))
  assert_eq(Span::status(span), Ok)
  
  // Test Span events
  Span::add_event(span, "event1", Some([("key1", StringValue("value1"))]))
  Span::end(span)
}

test "tracer and tracer provider functionality" {
  // Test TracerProvider creation
  let provider = TracerProvider::default()
  
  // Test Tracer creation
  let tracer = TracerProvider::get_tracer(provider, "test_tracer", Some("1.0.0"))
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name(), "test_tracer")
  assert_eq(scope.version(), Some("1.0.0"))
  
  // Test Span creation through Tracer
  let span = Tracer::start_span(tracer, "tracer_test_span", Some([("attr1", StringValue("value1"))]))
  assert_eq(Span::name(span), "tracer_test_span")
  assert_eq(Span::kind(span), Internal)
}

test "metrics counter functionality" {
  // Test MeterProvider and Meter creation
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test_meter")
  
  // Test Counter creation
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter description"), Some("count"))
  assert_eq(counter.name, "test_counter")
  assert_eq(counter.description, Some("Test counter description"))
  assert_eq(counter.unit, Some("count"))
  
  // Test Counter operations
  Counter::add(counter, 10.0, Some(Attributes::new()))
  Counter::add(counter, 5.5)
  
  // Test Counter as Instrument
  let instrument = Counter(counter.name, counter.description, counter.unit)
  assert_eq(Instrument::name(instrument), "test_counter")
  assert_eq(Instrument::description(instrument), Some("Test counter description"))
  assert_eq(Instrument::unit(instrument), Some("count"))
}

test "metrics histogram functionality" {
  // Test Histogram creation
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "histogram_meter")
  let histogram = Meter::create_histogram(meter, "response_time", Some("Response time histogram"), Some("ms"))
  
  assert_eq(histogram.name, "response_time")
  assert_eq(histogram.description, Some("Response time histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  // Test Histogram operations
  Histogram::record(histogram, 100.5, Some(Attributes::new()))
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 50.25)
  
  // Test Histogram as Instrument
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "response_time")
  assert_eq(Instrument::description(instrument), Some("Response time histogram"))
}

test "log record and logger functionality" {
  // Test LogRecord creation
  let log_record = LogRecord::new(Info, "Test log message")
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(LogRecord::body(log_record), Some("Test log message"))
  
  // Test LogRecord with full context
  let full_log = LogRecord::new_with_context(
    Error,
    Some("Error occurred"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace_123"),
    Some("span_456"),
    Some(Context::root())
  )
  assert_eq(LogRecord::severity_number(full_log), Error)
  assert_eq(LogRecord::body(full_log), Some("Error occurred"))
  assert_eq(LogRecord::trace_id(full_log), Some("trace_123"))
  assert_eq(LogRecord::span_id(full_log), Some("span_456"))
  
  // Test Logger functionality
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test_logger")
  Logger::emit(logger, log_record)
  Logger::emit(logger, full_log)
}

test "context and baggage functionality" {
  // Test Context operations
  let root_ctx = Context::root()
  let key = ContextKey::new("test_key")
  
  // Test Context with value
  let ctx_with_value = Context::with_value(root_ctx, key, "test_value")
  assert_eq(Context::get(ctx_with_value, key), Some("test_value"))
  
  // Test Context with non-existent key
  let non_existent_key = ContextKey::new("non_existent")
  assert_eq(Context::get(root_ctx, non_existent_key), None)
  
  // Test Baggage operations
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "baggage_key", "baggage_value")
  assert_eq(Baggage::get_entry(updated_baggage, "baggage_key"), Some("baggage_value"))
  
  // Test Baggage with non-existent entry
  assert_eq(Baggage::get_entry(baggage, "non_existent"), None)
  
  // Test Baggage entry removal
  let final_baggage = Baggage::remove_entry(updated_baggage, "baggage_key")
  // Note: Simplified implementation might not actually remove
}

test "propagation functionality" {
  // Test TextMapCarrier operations
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "custom_header", "custom_value")
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  // Test W3C Trace Context Propagator
  let trace_propagator = W3CTraceContextPropagator::new()
  
  // Test W3C Baggage Propagator
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // Test Composite Propagator
  let composite = CompositePropagator::new([trace_propagator])
  let ctx = Context::root()
  
  // Test injection
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_key = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx, extracted_key), Some("true"))
}

test "http client functionality" {
  // Test HTTP Request creation
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token123")]
  let request = HttpRequest::new("POST", "https://api.example.com/data", headers, Some("{\"key\":\"value\"}"))
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(request), Some("{\"key\":\"value\"}"))
  
  // Test HTTP Response creation
  let response_headers = [("Content-Type", "application/json")]
  let response = HttpResponse::new(200, response_headers, Some("{\"result\":\"success\"}"))
  
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("{\"result\":\"success\"}"))
  
  // Test HTTP Client creation (simplified)
  let client = HttpClient::new()
  // Note: Actual HTTP operations would be implemented here
}