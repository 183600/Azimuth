// Main file to test our azimuth implementation
fn main {
  // Test basic functionality
  let span_ctx = SpanContext::new("trace_001", "span_001", true, "active")
  let span = Span::new("test_span", Internal, span_ctx)
  
  // Verify span properties
  assert_eq(Span::name(span), "test_span")
  assert_true(Span::is_recording(span))
  
  // Test context operations
  let root_ctx = Context::root()
  let key = ContextKey::new("test.key")
  let ctx_with_value = Context::with_value(root_ctx, key, "test_value")
  
  match Context::get(ctx_with_value, key) {
    Some(value) => assert_eq(value, "test_value")
    None => assert_true(false)
  }
  
  // Test attribute values
  let string_attr = StringValue("test_string")
  match string_attr {
    StringValue(value) => assert_eq(value, "test_string")
    _ => assert_true(false)
  }
  
  // Test metrics
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  let counter = Meter::create_counter(meter, "test_counter")
  Counter::add(counter, 10.0)
  
  // Test logs
  let log_record = LogRecord::new(Info, "Test log message")
  assert_eq(LogRecord::severity_number(log_record), Info)
  match LogRecord::body(log_record) {
    Some(body) => assert_eq(body, "Test log message")
    None => assert_true(false)
  }
  
  // Test HTTP client
  let client = HttpClient::new()
  let request = HttpRequest::new("GET", "https://example.com", [], None)
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://example.com")
  
  let response = HttpResponse::new(200, [], Some("response body"))
  assert_eq(HttpResponse::status_code(response), 200)
  match HttpResponse::body(response) {
    Some(body) => assert_eq(body, "response body")
    None => assert_true(false)
  }
  
  println("All tests passed!")
}