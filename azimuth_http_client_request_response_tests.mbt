// Azimuth HTTP客户端和请求响应处理测试
// 测试HTTP客户端、请求和响应的基本功能

test "HTTP客户端基本请求创建" {
  // 创建HTTP客户端
  let client = HttpClient::new()
  
  // 创建HTTP请求
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token123")]
  let request = HttpRequest::new("GET", "https://api.example.com/data", headers, Some("{\"query\":\"test\"}"))
  
  // 验证请求属性
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  match HttpRequest::body(request) {
    Some(body) => assert_eq(body, "{\"query\":\"test\"}")
    None => assert_true(false)
  }
}

test "HTTP响应处理和状态码验证" {
  // 创建HTTP响应
  let response_headers = [("Content-Type", "application/json"), ("X-Request-ID", "req-12345")]
  let response = HttpResponse::new(200, response_headers, Some("{\"status\":\"success\",\"data\":[1,2,3]}"))
  
  // 验证响应属性
  assert_eq(HttpResponse::status_code(response), 200)
  match HttpResponse::body(response) {
    Some(body) => {
      assert_true(body.contains("success"))
      assert_true(body.contains("[1,2,3]"))
    }
    None => assert_true(false)
  }
}

test "HTTP错误响应处理" {
  // 创建错误响应
  let error_response = HttpResponse::new(404, [], Some("{\"error\":\"Not Found\",\"message\":\"Resource not found\"}"))
  
  // 验证错误响应
  assert_eq(HttpResponse::status_code(error_response), 404)
  match HttpResponse::body(error_response) {
    Some(body) => {
      assert_true(body.contains("Not Found"))
      assert_true(body.contains("Resource not found"))
    }
    None => assert_true(false)
  }
}

test "HTTP请求头处理" {
  // 测试复杂请求头
  let complex_headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer abc123def456"),
    ("X-Custom-Header", "custom-value"),
    ("User-Agent", "Azimuth-Telemetry/1.0.0"),
    ("Accept", "application/json,text/plain")
  ]
  
  let request = HttpRequest::new("POST", "https://api.example.com/submit", complex_headers, Some("{\"data\":\"test\"}"))
  
  // 验证请求方法和URL
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/submit")
  match HttpRequest::body(request) {
    Some(body) => assert_eq(body, "{\"data\":\"test\"}")
    None => assert_true(false)
  }
}

test "HTTP响应头处理" {
  // 测试复杂响应头
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Rate-Limit-Limit", "1000"),
    ("X-Rate-Limit-Remaining", "999"),
    ("X-Request-ID", "req-67890"),
    ("Cache-Control", "no-cache"),
    ("X-Response-Time", "125ms")
  ]
  
  let response = HttpResponse::new(200, response_headers, Some("{\"data\":\"processed\"}"))
  
  // 验证响应状态和内容
  assert_eq(HttpResponse::status_code(response), 200)
  match HttpResponse::body(response) {
    Some(body) => assert_eq(body, "{\"data\":\"processed\"}")
    None => assert_true(false)
  }
}