test "HTTP Client basic operations test" {
  // Test HTTP Client creation
  let client = HttpClient::new()
  @assertion.assert_true(true)?  // Basic creation test
  
  // Test HTTP Request creation and properties
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Request-ID", "req-456")
  ]
  let request = HttpRequest::new("GET", "https://api.example.com/users", headers, None)
  
  @assertion.assert_eq(request.http_method, "GET")?
  @assertion.assert_eq(request.url, "https://api.example.com/users")?
  @assertion.assert_eq(request.headers.length, 3)?
  @assertion.assert_eq(request.body, None)?
  
  // Test request with body
  let post_headers = [
    ("Content-Type", "application/json"),
    ("Accept", "application/json")
  ]
  let request_body = "{\"name\":\"Test User\",\"email\":\"test@example.com\"}"
  let post_request = HttpRequest::new("POST", "https://api.example.com/users", post_headers, Some(request_body))
  
  @assertion.assert_eq(post_request.http_method, "POST")?
  @assertion.assert_eq(post_request.body?, "{\"name\":\"Test User\",\"email\":\"test@example.com\"}")?
  
  // Test HTTP Response creation and properties
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Response-Time", "150ms")
  ]
  let response_body = "{\"id\":123,\"name\":\"Test User\",\"created\":\"2025-01-01\"}"
  let response = HttpResponse::new(200, response_headers, Some(response_body))
  
  @assertion.assert_eq(response.status_code, 200)?
  @assertion.assert_eq(response.headers.length, 2)?
  @assertion.assert_eq(response.body?, "{\"id\":123,\"name\":\"Test User\",\"created\":\"2025-01-01\"}")?
  
  // Test error response
  let error_response = HttpResponse::new(404, [], Some("{\"error\":\"User not found\"}"))
  @assertion.assert_eq(error_response.status_code, 404)?
  @assertion.assert_eq(error_response.headers.length, 0)?
  @assertion.assert_eq(error_response.body?, "{\"error\":\"User not found\"}")?
}

test "HTTP Request methods and headers test" {
  // Test different HTTP methods
  let methods = ["GET", "POST", "PUT", "DELETE", "PATCH", "HEAD", "OPTIONS"]
  let base_url = "https://api.example.com/resource"
  
  for method in methods {
    let request = HttpRequest::new(method, base_url, [], None)
    @assertion.assert_eq(request.http_method, method)?
    @assertion.assert_eq(request.url, base_url)?
    @assertion.assert_eq(request.headers.length, 0)?
    @assertion.assert_eq(request.body, None)?
  }
  
  // Test complex headers
  let complex_headers = [
    ("Authorization", "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ"),
    ("Content-Type", "multipart/form-data; boundary=----WebKitFormBoundary7MA4YWxkTrZu0gW"),
    ("Accept", "application/json, text/plain, */*"),
    ("User-Agent", "Azimuth-Telemetry/1.0.0 (Linux; x64)"),
    ("X-Custom-Header", "custom-value-with-special-chars-àáâãä"),
    ("Cookie", "session_id=abc123; user_pref=dark_mode; csrf_token=xyz789"),
    ("X-Forwarded-For", "192.168.1.1, 10.0.0.1"),
    ("X-Request-ID", "req-123e4567-e89b-12d3-a456-426614174000")
  ]
  
  let complex_request = HttpRequest::new("GET", "https://api.example.com/test", complex_headers, None)
  @assertion.assert_eq(complex_request.headers.length, 8)?
  
  // Test request with different body types
  let json_body = "{\"key\":\"value\",\"number\":42,\"array\":[1,2,3]}"
  let xml_body = "<?xml version=\"1.0\"?><root><item>value</item></root>"
  let text_body = "This is plain text content"
  let empty_body = ""
  
  let json_request = HttpRequest::new("POST", "https://api.example.com/json", [], Some(json_body))
  let xml_request = HttpRequest::new("POST", "https://api.example.com/xml", [], Some(xml_body))
  let text_request = HttpRequest::new("POST", "https://api.example.com/text", [], Some(text_body))
  let empty_request = HttpRequest::new("POST", "https://api.example.com/empty", [], Some(empty_body))
  
  @assertion.assert_eq(json_request.body?, json_body)?
  @assertion.assert_eq(xml_request.body?, xml_body)?
  @assertion.assert_eq(text_request.body?, text_body)?
  @assertion.assert_eq(empty_request.body?, empty_body)?
}

test "HTTP Response status codes and handling test" {
  // Test different HTTP status codes
  let status_codes = [
    (200, "OK"),
    (201, "Created"),
    (204, "No Content"),
    (301, "Moved Permanently"),
    (302, "Found"),
    (400, "Bad Request"),
    (401, "Unauthorized"),
    (403, "Forbidden"),
    (404, "Not Found"),
    (500, "Internal Server Error"),
    (502, "Bad Gateway"),
    (503, "Service Unavailable")
  ]
  
  for (status_code, description) in status_codes {
    let response = HttpResponse::new(status_code, [], Some("{\"status\":\"" + description + "\"}"))
    @assertion.assert_eq(response.status_code, status_code)?
    @assertion.assert_eq(response.body?, "{\"status\":\"" + description + "\"}")?
  }
  
  // Test response with different content types
  let json_headers = [("Content-Type", "application/json")]
  let xml_headers = [("Content-Type", "application/xml")]
  let text_headers = [("Content-Type", "text/plain")]
  let html_headers = [("Content-Type", "text/html")]
  
  let json_response = HttpResponse::new(200, json_headers, Some("{\"data\":\"value\"}"))
  let xml_response = HttpResponse::new(200, xml_headers, Some("<root><data>value</data></root>"))
  let text_response = HttpResponse::new(200, text_headers, Some("Simple text response"))
  let html_response = HttpResponse::new(200, html_headers, Some("<html><body>HTML Response</body></html>"))
  
  @assertion.assert_eq(json_response.headers.length, 1)?
  @assertion.assert_eq(xml_response.headers.length, 1)?
  @assertion.assert_eq(text_response.headers.length, 1)?
  @assertion.assert_eq(html_response.headers.length, 1)?
  
  // Test response without body
  let no_body_response = HttpResponse::new(204, [], None)
  @assertion.assert_eq(no_body_response.status_code, 204)?
  @assertion.assert_eq(no_body_response.body, None)?
}

test "HTTP Client telemetry integration test" {
  // Test HTTP operations with telemetry context
  
  // Create a request with trace context
  let trace_headers = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("X-Request-ID", "req-12345"),
    ("User-Agent", "Azimuth-Telemetry/1.0.0")
  ]
  
  let telemetry_request = HttpRequest::new(
    "GET", 
    "https://api.example.com/telemetry", 
    trace_headers, 
    None
  )
  
  @assertion.assert_eq(telemetry_request.headers.length, 3)?
  let trace_header = telemetry_request.headers[0]
  @assertion.assert_eq(trace_header.0, "traceparent")?
  @assertion.assert_eq(trace_header.1, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")?
  
  // Simulate response with telemetry headers
  let telemetry_response_headers = [
    ("Content-Type", "application/json"),
    ("X-Response-Time", "45ms"),
    ("X-Trace-ID", "trace-12345"),
    ("X-Server-Timing", "database;dur=20,cache;dur=5")
  ]
  
  let telemetry_response = HttpResponse::new(
    200, 
    telemetry_response_headers, 
    Some("{\"data\":\"telemetry data\",\"metrics\":{\"count\":42}}")
  )
  
  @assertion.assert_eq(telemetry_response.status_code, 200)?
  @assertion.assert_eq(telemetry_response.headers.length, 4)?
  @assertion.assert_eq(telemetry_response.body?, "{\"data\":\"telemetry data\",\"metrics\":{\"count\":42}}")?
  
  // Verify response timing header
  let response_time = telemetry_response.headers[1]
  @assertion.assert_eq(response_time.0, "X-Response-Time")?
  @assertion.assert_eq(response_time.1, "45ms")?
  
  // Test error handling with telemetry
  let error_headers = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("X-Error-Code", "RATE_LIMITED")
  ]
  
  let error_response = HttpResponse::new(
    429, 
    error_headers, 
    Some("{\"error\":\"Rate limit exceeded\",\"retry_after\":60}")
  )
  
  @assertion.assert_eq(error_response.status_code, 429)?
  @assertion.assert_eq(error_response.body?, "{\"error\":\"Rate limit exceeded\",\"retry_after\":60}")?
}