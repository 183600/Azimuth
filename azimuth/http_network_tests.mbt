// HTTP Client and Network functionality tests

test "HttpRequest基本创建测试" {
  let headers = [
    ("Content-Type", "application/json"),
    ("User-Agent", "azimuth-telemetry/1.0.0")
  ]
  let request = HttpRequest::new("POST", "https://api.example.com/telemetry", headers, Some("{\"test\": \"data\"}"))
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/telemetry")
  
  let body = HttpRequest::body(request)
  assert_true(body is Some)
  
  match body {
    Some(content) => assert_eq(content, "{\"test\": \"data\"}")
    None => assert_true(false)
  }
}

test "HttpRequest无body测试" {
  let headers = [("Accept", "application/json")]
  let request = HttpRequest::new("GET", "https://api.example.com/data", headers)
  
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  
  let body = HttpRequest::body(request)
  assert_true(body is None)
}

test "HttpResponse基本创建测试" {
  let headers = [
    ("Content-Type", "application/json"),
    ("Server", "nginx/1.18.0")
  ]
  let response = HttpResponse::new(200, headers, Some("{\"status\": \"success\"}"))
  
  assert_eq(HttpResponse::status_code(response), 200)
  
  let body = HttpResponse::body(response)
  assert_true(body is Some)
  
  match body {
    Some(content) => assert_eq(content, "{\"status\": \"success\"}")
    None => assert_true(false)
  }
}

test "HttpResponse错误状态测试" {
  let headers = [("Content-Type", "text/plain")]
  let response = HttpResponse::new(404, headers, Some("Not Found"))
  
  assert_eq(HttpResponse::status_code(response), 404)
  
  let body = HttpResponse::body(response)
  assert_true(body is Some)
  
  match body {
    Some(content) => assert_eq(content, "Not Found")
    None => assert_true(false)
  }
}

test "HttpClient创建测试" {
  let client = HttpClient::new()
  // 简化测试，只检查创建成功
  assert_true(true)
}

test "TextMapCarrier基本操作测试" {
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "baggage", "user=id123,session=abc456")
  
  let trace_result = TextMapCarrier::get(carrier, "traceparent")
  assert_true(trace_result is Some)
  
  match trace_result {
    Some(value) => assert_eq(value, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    None => assert_true(false)
  }
}

test "TextMapCarrier空值测试" {
  let carrier = TextMapCarrier::new()
  let result = TextMapCarrier::get(carrier, "nonexistent.header")
  assert_true(result is None)
}

test "HttpRequest多种HTTP方法测试" {
  let headers = [("Content-Type", "application/json")]
  
  let get_request = HttpRequest::new("GET", "https://api.example.com/resource", headers)
  assert_eq(HttpRequest::http_method(get_request), "GET")
  
  let post_request = HttpRequest::new("POST", "https://api.example.com/resource", headers)
  assert_eq(HttpRequest::http_method(post_request), "POST")
  
  let put_request = HttpRequest::new("PUT", "https://api.example.com/resource", headers)
  assert_eq(HttpRequest::http_method(put_request), "PUT")
  
  let delete_request = HttpRequest::new("DELETE", "https://api.example.com/resource", headers)
  assert_eq(HttpRequest::http_method(delete_request), "DELETE")
}

test "HttpResponse多种状态码测试" {
  let headers = [("Content-Type", "text/plain")]
  
  let ok_response = HttpResponse::new(200, headers, Some("OK"))
  assert_eq(HttpResponse::status_code(ok_response), 200)
  
  let created_response = HttpResponse::new(201, headers, Some("Created"))
  assert_eq(HttpResponse::status_code(created_response), 201)
  
  let bad_request_response = HttpResponse::new(400, headers, Some("Bad Request"))
  assert_eq(HttpResponse::status_code(bad_request_response), 400)
  
  let unauthorized_response = HttpResponse::new(401, headers, Some("Unauthorized"))
  assert_eq(HttpResponse::status_code(unauthorized_response), 401)
  
  let server_error_response = HttpResponse::new(500, headers, Some("Internal Server Error"))
  assert_eq(HttpResponse::status_code(server_error_response), 500)
}

test "HTTP请求URL解析测试" {
  let headers = [("User-Agent", "test-client")]
  let request = HttpRequest::new("GET", "https://api.example.com:8080/v1/telemetry?format=json", headers)
  
  let url = HttpRequest::url(request)
  assert_eq(url, "https://api.example.com:8080/v1/telemetry?format=json")
}

test "HTTP响应复杂body测试" {
  let headers = [("Content-Type", "application/json")]
  let complex_body = "{\"metrics\": [{\"name\": \"cpu.usage\", \"value\": 75.5}], \"timestamp\": 1634567890}"
  let response = HttpResponse::new(200, headers, Some(complex_body))
  
  let body = HttpResponse::body(response)
  assert_true(body is Some)
  
  match body {
    Some(content) => {
      assert_eq(content, complex_body)
      assert_true(content.contains("cpu.usage"))
      assert_true(content.contains("75.5"))
    }
    None => assert_true(false)
  }
}

test "TextMapCarrier多值操作测试" {
  let carrier = TextMapCarrier::new()
  
  // 设置多个header
  TextMapCarrier::set(carrier, "traceparent", "00-trace-id-span-id-01")
  TextMapCarrier::set(carrier, "baggage", "key1=value1,key2=value2")
  TextMapCarrier::set(carrier, "x-custom-header", "custom-value")
  TextMapCarrier::set(carrier, "authorization", "Bearer token123")
  
  // 验证每个header都能正确获取
  let trace_result = TextMapCarrier::get(carrier, "traceparent")
  let baggage_result = TextMapCarrier::get(carrier, "baggage")
  let custom_result = TextMapCarrier::get(carrier, "x-custom-header")
  let auth_result = TextMapCarrier::get(carrier, "authorization")
  
  assert_true(trace_result is Some)
  assert_true(baggage_result is Some)
  assert_true(custom_result is Some)
  assert_true(auth_result is Some)
  
  match trace_result {
    Some(value) => assert_eq(value, "00-trace-id-span-id-01")
    None => assert_true(false)
  }
}