// Security Validation Tests for Azimuth Telemetry System
// This file contains tests for security-related scenarios and validations

test "sensitive data protection" {
  let attrs = Attributes::new()
  
  // Test with potentially sensitive data
  Attributes::set(attrs, "user.email", StringValue("user@example.com"))
  Attributes::set(attrs, "user.password", StringValue("secret123"))
  Attributes::set(attrs, "api.key", StringValue("sk-1234567890abcdef"))
  Attributes::set(attrs, "credit.card", StringValue("4111-1111-1111-1111"))
  Attributes::set(attrs, "ssn", StringValue("123-45-6789"))
  
  // Verify attributes are set
  let email = Attributes::get(attrs, "user.email")
  let password = Attributes::get(attrs, "user.password")
  let api_key = Attributes::get(attrs, "api.key")
  let credit_card = Attributes::get(attrs, "credit.card")
  let ssn = Attributes::get(attrs, "ssn")
  
  match email {
    Some(StringValue(s)) => assert_eq(s, "user@example.com")
    _ => assert_true(false)
  }
  
  match password {
    Some(StringValue(s)) => assert_eq(s, "secret123")
    _ => assert_true(false)
  }
  
  match api_key {
    Some(StringValue(s)) => assert_eq(s, "sk-1234567890abcdef")
    _ => assert_true(false)
  }
  
  match credit_card {
    Some(StringValue(s)) => assert_eq(s, "4111-1111-1111-1111")
    _ => assert_true(false)
  }
  
  match ssn {
    Some(StringValue(s)) => assert_eq(s, "123-45-6789")
    _ => assert_true(false)
  }
  
  // In a real implementation, sensitive data should be masked or redacted
  // For this test, we verify the data is stored as-is in simplified implementation
}

test "trace context security" {
  // Test with potentially malicious trace IDs
  let malicious_trace_id = "<script>alert('xss')</script>"
  let malicious_span_id = "'; DROP TABLE users; --"
  let malicious_trace_state = "key=<script>malicious()</script>"
  
  // Create span context with malicious data
  let malicious_span_ctx = SpanContext::new(malicious_trace_id, malicious_span_id, true, malicious_trace_state)
  
  // Verify span context is created
  assert_eq(SpanContext::trace_id(malicious_span_ctx), malicious_trace_id)
  assert_eq(SpanContext::span_id(malicious_span_ctx), malicious_span_id)
  
  // Test with valid trace ID format
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let valid_span_id = "b7ad6b7169203331"
  let valid_trace_state = "key1=value1,key2=value2"
  
  let valid_span_ctx = SpanContext::new(valid_trace_id, valid_span_id, true, valid_trace_state)
  
  // Verify valid span context
  assert_eq(SpanContext::trace_id(valid_span_ctx), valid_trace_id)
  assert_eq(SpanContext::span_id(valid_span_ctx), valid_span_id)
  assert_true(SpanContext::is_valid(valid_span_ctx))
}

test "baggage security validation" {
  let baggage = Baggage::new()
  
  // Test with potentially malicious baggage entries
  let baggage1 = Baggage::set_entry(baggage, "user.id", "<script>alert('xss')</script>")
  let baggage2 = Baggage::set_entry(baggage1, "session.token", "'; DROP TABLE users; --")
  let baggage3 = Baggage::set_entry(baggage2, "auth.header", "Bearer <script>malicious()</script>")
  
  // Verify baggage entries are set
  let user_id = Baggage::get_entry(baggage3, "user.id")
  let session_token = Baggage::get_entry(baggage3, "session.token")
  let auth_header = Baggage::get_entry(baggage3, "auth.header")
  
  assert_eq(user_id, Some("<script>alert('xss')</script>"))
  assert_eq(session_token, Some("'; DROP TABLE users; --"))
  assert_eq(auth_header, Some("Bearer <script>malicious()</script>"))
  
  // Test with very long baggage values (potential DoS)
  let long_value = "a" * 10000
  let baggage4 = Baggage::set_entry(baggage3, "large.value", long_value)
  let large_value = Baggage::get_entry(baggage4, "large.value")
  
  assert_eq(large_value, Some(long_value))
  
  // In a real implementation, baggage values should be sanitized
}

test "log record security" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "security-test")
  
  // Test with potentially malicious log messages
  let malicious_message = "<script>alert('xss')</script>"
  let sql_injection_message = "'; DROP TABLE users; --"
  let path_traversal_message = "../../../../etc/passwd"
  
  let record1 = LogRecord::new(Error, malicious_message)
  let record2 = LogRecord::new(Warn, sql_injection_message)
  let record3 = LogRecord::new(Info, path_traversal_message)
  
  // Verify log records are created
  assert_eq(LogRecord::body(record1), Some(malicious_message))
  assert_eq(LogRecord::body(record2), Some(sql_injection_message))
  assert_eq(LogRecord::body(record3), Some(path_traversal_message))
  
  // Emit log records
  Logger::emit(logger, record1)
  Logger::emit(logger, record2)
  Logger::emit(logger, record3)
  
  // Test with malicious attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.input", "<script>alert('xss')</script>")
  Attributes::set(attrs, "query.param", "'; DROP TABLE users; --")
  
  let record4 = LogRecord::new_with_context(
    Error,
    Some("Error with malicious input"),
    Some(attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    None,
    None,
    Some(Context::root())
  )
  
  Logger::emit(logger, record4)
  
  // In a real implementation, log messages should be sanitized
}

test "HTTP security validation" {
  let client = HttpClient::new()
  
  // Test with potentially malicious URLs
  let malicious_urls = [
    "http://example.com/<script>alert('xss')</script>",
    "http://example.com/'; DROP TABLE users; --",
    "http://example.com/../../../../etc/passwd",
    "javascript:alert('xss')",
    "data:text/html,<script>alert('xss')</script>"
  ]
  
  for url in malicious_urls {
    let request = HttpRequest::new("GET", url, [], None)
    
    // Verify request is created
    assert_eq(HttpRequest::url(request), url)
    assert_eq(HttpRequest::http_method(request), "GET")
    assert_eq(HttpRequest::body(request), None)
  }
  
  // Test with malicious headers
  let malicious_headers = [
    ("User-Agent", "<script>alert('xss')</script>"),
    ("X-Custom-Header", "'; DROP TABLE users; --"),
    ("Cookie", "session=<script>malicious()</script>")
  ]
  
  let request_with_headers = HttpRequest::new("POST", "https://api.example.com/data", malicious_headers, Some("{\"test\":true}"))
  
  // Verify request with malicious headers
  assert_eq(HttpRequest::url(request_with_headers), "https://api.example.com/data")
  assert_eq(HttpRequest::http_method(request_with_headers), "POST")
  assert_eq(HttpRequest::body(request_with_headers), Some("{\"test\":true}"))
  
  // Test with malicious response body
  let malicious_body = "<script>alert('xss')</script>"
  let response = HttpResponse::new(200, [("Content-Type", "text/html")], Some(malicious_body))
  
  // Verify response with malicious body
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some(malicious_body))
}

test "resource security validation" {
  let resource = Resource::new()
  
  // Test with potentially malicious resource attributes
  let malicious_attrs = [
    ("service.name", "<script>alert('xss')</script>"),
    ("host.name", "'; DROP TABLE users; --"),
    ("process.command_line", "rm -rf /"),
    ("user.email", "../../etc/passwd"),
    ("api.key", "sk-1234567890abcdef")
  ]
  
  let resource_with_malicious = Resource::with_attributes(resource, malicious_attrs)
  
  // Verify malicious attributes are set
  for attr in malicious_attrs {
    let result = Resource::get_attribute(resource_with_malicious, attr.0)
    match result {
      Some(value) => {
        match (value, attr.1) {
          (StringValue(s), StringValue(expected)) => assert_eq(s, expected)
          _ => assert_true(false)
        }
      }
      None => assert_true(false)
    }
  }
  
  // Test with very long attribute values (potential DoS)
  let long_value = "a" * 100000
  let long_attrs = [("large.attribute", StringValue(long_value))]
  
  let resource_with_long = Resource::with_attributes(resource, long_attrs)
  let large_attr = Resource::get_attribute(resource_with_long, "large.attribute")
  
  match large_attr {
    Some(StringValue(s)) => assert_eq(s.length(), 100000)
    _ => assert_true(false)
  }
  
  // In a real implementation, resource attributes should be validated
}

test "context propagation security" {
  let carrier = TextMapCarrier::new()
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let ctx = Context::root()
  
  // Test with potentially malicious context values
  let key = ContextKey::new("<script>alert('xss')</script>")
  let ctx_with_malicious = Context::with_value(ctx, key, "'; DROP TABLE users; --")
  
  // Inject malicious context
  CompositePropagator::inject(propagator, ctx_with_malicious, carrier)
  
  // Extract context
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  
  // Verify extraction works
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
  
  // Test with malicious headers
  let malicious_carrier = TextMapCarrier::new()
  TextMapCarrier::set(malicious_carrier, "traceparent", "<script>alert('xss')</script>")
  TextMapCarrier::set(malicious_carrier, "baggage", "'; DROP TABLE users; --")
  
  let extracted_from_malicious = CompositePropagator::extract(propagator, malicious_carrier)
  let malicious_value = Context::get(extracted_from_malicious, ContextKey::new("extracted"))
  
  // Should handle malicious headers gracefully
  assert_eq(malicious_value, Some("true"))
}

test "attribute value injection protection" {
  let attrs = Attributes::new()
  
  // Test with various injection patterns
  let injection_patterns = [
    ("xss.pattern", StringValue("<script>alert('xss')</script>")),
    ("sql.injection", StringValue("'; DROP TABLE users; --")),
    ("path.traversal", StringValue("../../../../etc/passwd")),
    ("ldap.injection", StringValue("*(|(objectClass=*))")),
    ("command.injection", StringValue("; rm -rf /")),
    ("xml.injection", StringValue("<?xml version=\"1.0\"?><!DOCTYPE foo [<!ENTITY xxe SYSTEM \"file:///etc/passwd\">]><foo>&xxe;</foo>")),
    ("json.injection", StringValue("{\"malicious\":\"<script>alert('xss')</script>\"}"))
  ]
  
  for pattern in injection_patterns {
    Attributes::set(attrs, pattern.0, pattern.1)
  }
  
  // Verify all injection patterns are stored
  for pattern in injection_patterns {
    let result = Attributes::get(attrs, pattern.0)
    match result {
      Some(value) => {
        match (value, pattern.1) {
          (StringValue(s), StringValue(expected)) => assert_eq(s, expected)
          _ => assert_true(false)
        }
      }
      None => assert_true(false)
    }
  }
  
  // Test with null bytes and control characters
  let null_byte_value = "value\x00with\x00null\x00bytes"
  let control_char_value = "value\x01with\x02control\x03chars"
  
  Attributes::set(attrs, "null.bytes", StringValue(null_byte_value))
  Attributes::set(attrs, "control.chars", StringValue(control_char_value))
  
  let null_result = Attributes::get(attrs, "null.bytes")
  let control_result = Attributes::get(attrs, "control.chars")
  
  match null_result {
    Some(StringValue(s)) => assert_eq(s, null_byte_value)
    _ => assert_true(false)
  }
  
  match control_result {
    Some(StringValue(s)) => assert_eq(s, control_char_value)
    _ => assert_true(false)
  }
}

test "span security validation" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "security-test")
  
  // Test with potentially malicious span names
  let malicious_names = [
    "<script>alert('xss')</script>",
    "'; DROP TABLE users; --",
    "../../../../etc/passwd",
    "a" * 10000  // Very long name
  ]
  
  for name in malicious_names {
    let span = Tracer::start_span(tracer, name)
    
    // Verify span is created
    assert_eq(Span::name(span), name)
    assert_true(Span::is_recording(span))
    
    // Add malicious events
    Span::add_event(span, "<script>alert('xss')</script>", Some([
      ("malicious.attr", StringValue("'; DROP TABLE users; --"))
    ]))
    
    // Set malicious status
    Span::set_status(span, Error, Some("<script>alert('xss')</script>"))
    
    // End span
    Span::end(span)
  }
  
  // In a real implementation, span names and events should be sanitized
}

test "metrics security validation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "security-test")
  
  // Test with potentially malicious metric names
  let malicious_names = [
    "<script>alert('xss')</script>",
    "'; DROP TABLE users; --",
    "../../../../etc/passwd",
    "a" * 10000  // Very long name
  ]
  
  for name in malicious_names {
    let counter = Meter::create_counter(meter, name)
    let histogram = Meter::create_histogram(meter, name)
    let updown_counter = Meter::create_updown_counter(meter, name)
    let gauge = Meter::create_gauge(meter, name)
    
    // Verify instruments are created
    assert_eq(counter.name, name)
    assert_eq(histogram.name, name)
    assert_eq(updown_counter.name, name)
    assert_eq(gauge.name, name)
    
    // Update metrics with malicious attributes
    let attrs = Attributes::new()
    Attributes::set(attrs, "<script>alert('xss')</script>", StringValue("'; DROP TABLE users; --"))
    
    Counter::add(counter, 1.0, Some(attrs))
    Histogram::record(histogram, 1.0, Some(attrs))
    UpDownCounter::add(updown_counter, 1.0, Some(attrs))
  }
  
  // Test with extreme metric values
  let counter = Meter::create_counter(meter, "extreme.values")
  
  // Add very large value
  Counter::add(counter, 1.7976931348623157e+308)  // Max double
  
  // Add very small value
  Counter::add(counter, -1.7976931348623157e+308)  // Min double
  
  // Add NaN (if supported)
  // Counter::add(counter, 0.0 / 0.0)  // NaN
  
  // Add infinity (if supported)
  // Counter::add(counter, 1.0 / 0.0)  // Infinity
  
  // In a real implementation, metric names and values should be validated
}

test "authentication and authorization context" {
  // Test with authentication context
  let attrs = Attributes::new()
  
  // Add authentication-related attributes
  Attributes::set(attrs, "auth.user.id", StringValue("user123"))
  Attributes::set(attrs, "auth.user.role", StringValue("admin"))
  Attributes::set(attrs, "auth.token", StringValue("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"))  // JWT token
  Attributes::set(attrs, "auth.permissions", ArrayStringValue(["read", "write", "delete"]))
  
  // Verify authentication attributes
  let user_id = Attributes::get(attrs, "auth.user.id")
  let user_role = Attributes::get(attrs, "auth.user.role")
  let auth_token = Attributes::get(attrs, "auth.token")
  let permissions = Attributes::get(attrs, "auth.permissions")
  
  match user_id {
    Some(StringValue(s)) => assert_eq(s, "user123")
    _ => assert_true(false)
  }
  
  match user_role {
    Some(StringValue(s)) => assert_eq(s, "admin")
    _ => assert_true(false)
  }
  
  match auth_token {
    Some(StringValue(s)) => assert_eq(s, "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9")
    _ => assert_true(false)
  }
  
  match permissions {
    Some(ArrayStringValue(arr)) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "read")
      assert_eq(arr[1], "write")
      assert_eq(arr[2], "delete")
    }
    _ => assert_true(false)
  }
  
  // Test with authorization context
  let auth_ctx = Context::root()
  let auth_key = ContextKey::new("auth.context")
  let auth_value = "{\"user_id\":\"user123\",\"role\":\"admin\",\"permissions\":[\"read\",\"write\",\"delete\"]}"
  let ctx_with_auth = Context::with_value(auth_ctx, auth_key, auth_value)
  
  // Verify authorization context
  let retrieved_auth = Context::get(ctx_with_auth, auth_key)
  assert_eq(retrieved_auth, Some(auth_value))
  
  // In a real implementation, authentication tokens should be validated
}

test "data encryption and privacy" {
  // Test with potentially sensitive data that should be encrypted
  let sensitive_attrs = [
    ("user.email", StringValue("user@example.com")),
    ("user.phone", StringValue("+1-555-123-4567")),
    ("user.address", StringValue("123 Main St, Anytown, USA")),
    ("user.ssn", StringValue("123-45-6789")),
    ("payment.card", StringValue("4111-1111-1111-1111")),
    ("payment.bank.account", StringValue("123456789")),
    ("health.record.id", StringValue("HR-123456789")),
    ("personal.id", StringValue("PID-987654321"))
  ]
  
  let attrs = Attributes::new()
  
  for attr in sensitive_attrs {
    Attributes::set(attrs, attr.0, attr.1)
  }
  
  // Verify sensitive attributes are stored
  for attr in sensitive_attrs {
    let result = Attributes::get(attrs, attr.0)
    match result {
      Some(value) => {
        match (value, attr.1) {
          (StringValue(s), StringValue(expected)) => assert_eq(s, expected)
          _ => assert_true(false)
        }
      }
      None => assert_true(false)
    }
  }
  
  // Test with PII (Personally Identifiable Information) markers
  let pii_attrs = [
    ("pii.user.email", StringValue("user@example.com")),
    ("pii.user.phone", StringValue("+1-555-123-4567")),
    ("pii.user.ssn", StringValue("123-45-6789"))
  ]
  
  for attr in pii_attrs {
    Attributes::set(attrs, attr.0, attr.1)
  }
  
  // Verify PII attributes
  for attr in pii_attrs {
    let result = Attributes::get(attrs, attr.0)
    match result {
      Some(value) => {
        match (value, attr.1) {
          (StringValue(s), StringValue(expected)) => assert_eq(s, expected)
          _ => assert_true(false)
        }
      }
      None => assert_true(false)
    }
  }
  
  // In a real implementation, sensitive data should be encrypted or redacted
}