// Error Handling and Boundary Condition Tests for Azimuth
// These tests verify error handling and edge cases

test "empty span context validation" {
  let empty_trace_id = ""
  let empty_span_id = ""
  let empty_ctx = SpanContext::new(empty_trace_id, empty_span_id, true, "")
  
  assert_false(SpanContext::is_valid(empty_ctx))
  
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let valid_span_id = "b7ad6b7169203331"
  let valid_ctx = SpanContext::new(valid_trace_id, valid_span_id, true, "")
  
  assert_true(SpanContext::is_valid(valid_ctx))
}

test "context with missing values" {
  let ctx = Context::root()
  let key = ContextKey::new("nonexistent.key")
  let value = Context::get(ctx, key)
  
  assert_eq(value, None)
}

test "attributes with missing keys" {
  let attrs = Attributes::new()
  let missing_value = Attributes::get(attrs, "nonexistent.key")
  
  assert_eq(missing_value, None)
}

test "resource with missing attributes" {
  let resource = Resource::new()
  let missing_attr = Resource::get_attribute(resource, "nonexistent.attr")
  
  assert_eq(missing_attr, None)
}

test "baggage with missing entries" {
  let baggage = Baggage::new()
  let missing_entry = Baggage::get_entry(baggage, "nonexistent.key")
  
  assert_eq(missing_entry, None)
}

test "text map carrier with missing headers" {
  let carrier = TextMapCarrier::new()
  let missing_header = TextMapCarrier::get(carrier, "nonexistent-header")
  
  assert_eq(missing_header, None)
}

test "log record with optional fields" {
  let minimal_record = LogRecord::new(Info, "Minimal message")
  
  assert_eq(LogRecord::severity_number(minimal_record), Info)
  assert_eq(LogRecord::body(minimal_record), Some("Minimal message"))
  assert_eq(LogRecord::trace_id(minimal_record), None)
  assert_eq(LogRecord::span_id(minimal_record), None)
  
  let record_with_trace = LogRecord::new_with_context(
    Error,
    Some("Error message"),
    None,
    None,
    None,
    Some("trace123"),
    None,
    None
  )
  
  assert_eq(LogRecord::trace_id(record_with_trace), Some("trace123"))
  assert_eq(LogRecord::span_id(record_with_trace), None)
}

test "instrument creation with minimal parameters" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  let minimal_counter = Meter::create_counter(meter, "minimal.counter")
  assert_eq(minimal_counter.name, "minimal.counter")
  assert_eq(minimal_counter.description, None)
  assert_eq(minimal_counter.unit, None)
  
  let minimal_histogram = Meter::create_histogram(meter, "minimal.histogram")
  assert_eq(minimal_histogram.name, "minimal.histogram")
  assert_eq(minimal_histogram.description, None)
  assert_eq(minimal_histogram.unit, None)
  
  let minimal_updown_counter = Meter::create_updown_counter(meter, "minimal.updown")
  assert_eq(minimal_updown_counter.name, "minimal.updown")
  assert_eq(minimal_updown_counter.description, None)
  assert_eq(minimal_updown_counter.unit, None)
  
  let minimal_gauge = Meter::create_gauge(meter, "minimal.gauge")
  assert_eq(minimal_gauge.name, "minimal.gauge")
  assert_eq(minimal_gauge.description, None)
  assert_eq(minimal_gauge.unit, None)
}

test "span with different kinds" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  
  let internal_span = Span::new("internal", Internal, span_ctx)
  let server_span = Span::new("server", Server, span_ctx)
  let client_span = Span::new("client", Client, span_ctx)
  let producer_span = Span::new("producer", Producer, span_ctx)
  let consumer_span = Span::new("consumer", Consumer, span_ctx)
  
  assert_eq(Span::kind(internal_span), Internal)
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::kind(client_span), Client)
  assert_eq(Span::kind(producer_span), Producer)
  assert_eq(Span::kind(consumer_span), Consumer)
}

test "status code transitions" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("test-span", Internal, span_ctx)
  
  // Initial status should be Unset
  assert_eq(Span::status(span), Unset)
  
  // Set to Ok
  Span::set_status(span, Ok)
  assert_eq(Span::status(span), Ok)  // Note: simplified implementation
  
  // Set to Error
  Span::set_status(span, Error, Some("Something went wrong"))
  assert_eq(Span::status(span), Error)  // Note: simplified implementation
}

test "severity number levels" {
  let trace_record = LogRecord::new(Trace, "Trace message")
  let debug_record = LogRecord::new(Debug, "Debug message")
  let info_record = LogRecord::new(Info, "Info message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  let fatal_record = LogRecord::new(Fatal, "Fatal message")
  
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
}

test "instrumentation scope with optional fields" {
  // Test with all fields
  let full_scope = InstrumentationScope::{ 
    name: "test-scope", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/schema") 
  }
  
  assert_eq(full_scope.name, "test-scope")
  assert_eq(full_scope.version, Some("1.0.0"))
  assert_eq(full_scope.schema_url, Some("https://example.com/schema"))
  
  // Test with minimal fields
  let minimal_scope = InstrumentationScope::{ 
    name: "minimal-scope", 
    version: None, 
    schema_url: None 
  }
  
  assert_eq(minimal_scope.name, "minimal-scope")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
}

test "http operations with empty bodies" {
  let headers = [("Content-Type", "application/json")]
  let request_with_body = HttpRequest::new("POST", "https://api.example.com/data", headers, Some("{\"key\":\"value\"}"))
  let request_without_body = HttpRequest::new("GET", "https://api.example.com/data", headers)
  
  assert_eq(HttpRequest::http_method(request_with_body), "POST")
  assert_eq(HttpRequest::body(request_with_body), Some("{\"key\":\"value\"}"))
  
  assert_eq(HttpRequest::http_method(request_without_body), "GET")
  assert_eq(HttpRequest::body(request_without_body), None)
  
  let response_with_body = HttpResponse::new(200, headers, Some("{\"result\":\"success\"}"))
  let response_without_body = HttpResponse::new(204, headers)
  
  assert_eq(HttpResponse::status_code(response_with_body), 200)
  assert_eq(HttpResponse::body(response_with_body), Some("{\"result\":\"success\"}"))
  
  assert_eq(HttpResponse::status_code(response_without_body), 204)
  assert_eq(HttpResponse::body(response_without_body), None)
}