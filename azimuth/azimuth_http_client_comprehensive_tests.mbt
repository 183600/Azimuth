// Comprehensive HTTP client tests for Azimuth telemetry system
import "azimuth/azimuth"

// Test 1: Basic HTTP request handling
pub test "基本HTTP请求处理测试" {
  let client = azimuth::HttpClient::new()
  
  // 创建GET请求
  let headers = [("Content-Type", "application/json"), ("Accept", "application/json")]
  let request = azimuth::HttpRequest::new("GET", "https://api.example.com/users", headers, None)
  
  // 验证请求属性
  assert_eq(azimuth::HttpRequest::http_method(request), "GET")
  assert_eq(azimuth::HttpRequest::url(request), "https://api.example.com/users")
  assert_eq(azimuth::HttpRequest::body(request), None)
  
  // 创建响应
  let response_headers = [("Content-Type", "application/json"), ("Content-Length", "1024")]
  let response = azimuth::HttpResponse::new(200, response_headers, Some("{\"users\": []}"))
  
  // 验证响应属性
  assert_eq(azimuth::HttpResponse::status_code(response), 200)
  assert_eq(azimuth::HttpResponse::body(response), Some("{\"users\": []}"))
}

// Test 2: HTTP methods testing
pub test "HTTP方法测试" {
  let client = azimuth::HttpClient::new()
  let headers = [("Content-Type", "application/json")]
  
  // GET请求
  let get_request = azimuth::HttpRequest::new("GET", "https://api.example.com/resource", headers, None)
  assert_eq(azimuth::HttpRequest::http_method(get_request), "GET")
  
  // POST请求
  let post_request = azimuth::HttpRequest::new("POST", "https://api.example.com/resource", headers, Some("{\"data\": \"value\"}"))
  assert_eq(azimuth::HttpRequest::http_method(post_request), "POST")
  assert_eq(azimuth::HttpRequest::body(post_request), Some("{\"data\": \"value\"}"))
  
  // PUT请求
  let put_request = azimuth::HttpRequest::new("PUT", "https://api.example.com/resource/1", headers, Some("{\"updated\": \"value\"}"))
  assert_eq(azimuth::HttpRequest::http_method(put_request), "PUT")
  assert_eq(azimuth::HttpRequest::body(put_request), Some("{\"updated\": \"value\"}"))
  
  // DELETE请求
  let delete_request = azimuth::HttpRequest::new("DELETE", "https://api.example.com/resource/1", headers, None)
  assert_eq(azimuth::HttpRequest::http_method(delete_request), "DELETE")
  
  // PATCH请求
  let patch_request = azimuth::HttpRequest::new("PATCH", "https://api.example.com/resource/1", headers, Some("{\"patch\": \"value\"}"))
  assert_eq(azimuth::HttpRequest::http_method(patch_request), "PATCH")
}

// Test 3: HTTP headers handling
pub test "HTTP头部处理测试" {
  let client = azimuth::HttpClient::new()
  
  // 多种头部类型
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("User-Agent", "Azimuth-Telemetry/1.0"),
    ("Accept", "application/json, text/plain"),
    ("X-Custom-Header", "custom-value"),
    ("X-Request-ID", "req-123456")
  ]
  
  let request = azimuth::HttpRequest::new("GET", "https://api.example.com/test", headers, None)
  
  // 验证请求创建
  assert_eq(azimuth::HttpRequest::http_method(request), "GET")
  assert_eq(azimuth::HttpRequest::url(request), "https://api.example.com/test")
  
  // 创建带有多种头部的响应
  let response_headers = [
    ("Content-Type", "application/json"),
    ("Content-Length", "512"),
    ("Cache-Control", "no-cache"),
    ("X-Rate-Limit-Remaining", "100"),
    ("X-Response-Time", "150ms"),
    ("Server", "nginx/1.18.0")
  ]
  
  let response = azimuth::HttpResponse::new(200, response_headers, Some("{\"status\": \"success\"}"))
  
  // 验证响应
  assert_eq(azimuth::HttpResponse::status_code(response), 200)
  assert_eq(azimuth::HttpResponse::body(response), Some("{\"status\": \"success\"}"))
}

// Test 4: HTTP status codes handling
pub test "HTTP状态码处理测试" {
  let client = azimuth::HttpClient::new()
  let headers = [("Content-Type", "application/json")]
  
  // 成功状态码
  let ok_response = azimuth::HttpResponse::new(200, headers, Some("{\"status\": \"OK\"}"))
  assert_eq(azimuth::HttpResponse::status_code(ok_response), 200)
  
  let created_response = azimuth::HttpResponse::new(201, headers, Some("{\"status\": \"Created\"}"))
  assert_eq(azimuth::HttpResponse::status_code(created_response), 201)
  
  let no_content_response = azimuth::HttpResponse::new(204, [], None)
  assert_eq(azimuth::HttpResponse::status_code(no_content_response), 204)
  assert_eq(azimuth::HttpResponse::body(no_content_response), None)
  
  // 重定向状态码
  let moved_response = azimuth::HttpResponse::new(301, [("Location", "https://new.example.com")], None)
  assert_eq(azimuth::HttpResponse::status_code(moved_response), 301)
  
  let found_response = azimuth::HttpResponse::new(302, [("Location", "https://temp.example.com")], None)
  assert_eq(azimuth::HttpResponse::status_code(found_response), 302)
  
  // 客户端错误状态码
  let bad_request_response = azimuth::HttpResponse::new(400, headers, Some("{\"error\": \"Bad Request\"}"))
  assert_eq(azimuth::HttpResponse::status_code(bad_request_response), 400)
  
  let unauthorized_response = azimuth::HttpResponse::new(401, headers, Some("{\"error\": \"Unauthorized\"}"))
  assert_eq(azimuth::HttpResponse::status_code(unauthorized_response), 401)
  
  let forbidden_response = azimuth::HttpResponse::new(403, headers, Some("{\"error\": \"Forbidden\"}"))
  assert_eq(azimuth::HttpResponse::status_code(forbidden_response), 403)
  
  let not_found_response = azimuth::HttpResponse::new(404, headers, Some("{\"error\": \"Not Found\"}"))
  assert_eq(azimuth::HttpResponse::status_code(not_found_response), 404)
  
  // 服务器错误状态码
  let internal_error_response = azimuth::HttpResponse::new(500, headers, Some("{\"error\": \"Internal Server Error\"}"))
  assert_eq(azimuth::HttpResponse::status_code(internal_error_response), 500)
  
  let bad_gateway_response = azimuth::HttpResponse::new(502, headers, Some("{\"error\": \"Bad Gateway\"}"))
  assert_eq(azimuth::HttpResponse::status_code(bad_gateway_response), 502)
}

// Test 5: HTTP request/response with different body types
pub test "不同请求体类型HTTP测试" {
  let client = azimuth::HttpClient::new()
  
  // JSON请求体
  let json_headers = [("Content-Type", "application/json")]
  let json_request = azimuth::HttpRequest::new(
    "POST", 
    "https://api.example.com/users", 
    json_headers, 
    Some("{\"name\": \"John Doe\", \"email\": \"john@example.com\"}")
  )
  assert_eq(azimuth::HttpRequest::body(json_request), Some("{\"name\": \"John Doe\", \"email\": \"john@example.com\"}"))
  
  // XML请求体
  let xml_headers = [("Content-Type", "application/xml")]
  let xml_request = azimuth::HttpRequest::new(
    "POST", 
    "https://api.example.com/data", 
    xml_headers, 
    Some("<?xml version=\"1.0\"?><data><item>test</item></data>")
  )
  assert_eq(azimuth::HttpRequest::body(xml_request), Some("<?xml version=\"1.0\"?><data><item>test</item></data>"))
  
  // 表单请求体
  let form_headers = [("Content-Type", "application/x-www-form-urlencoded")]
  let form_request = azimuth::HttpRequest::new(
    "POST", 
    "https://api.example.com/login", 
    form_headers, 
    Some("username=john&password=secret")
  )
  assert_eq(azimuth::HttpRequest::body(form_request), Some("username=john&password=secret"))
  
  // 纯文本请求体
  let text_headers = [("Content-Type", "text/plain")]
  let text_request = azimuth::HttpRequest::new(
    "POST", 
    "https://api.example.com/logs", 
    text_headers, 
    Some("This is a plain text log message")
  )
  assert_eq(azimuth::HttpRequest::body(text_request), Some("This is a plain text log message"))
  
  // 不同响应体类型
  let json_response = azimuth::HttpResponse::new(200, json_headers, Some("{\"result\": \"success\"}"))
  assert_eq(azimuth::HttpResponse::body(json_response), Some("{\"result\": \"success\"}"))
  
  let xml_response = azimuth::HttpResponse::new(200, xml_headers, Some("<?xml version=\"1.0\"?><result>success</result>"))
  assert_eq(azimuth::HttpResponse::body(xml_response), Some("<?xml version=\"1.0\"?><result>success</result>"))
  
  let text_response = azimuth::HttpResponse::new(200, text_headers, Some("Success response"))
  assert_eq(azimuth::HttpResponse::body(text_response), Some("Success response"))
}

// Test 6: HTTP client with telemetry integration
pub test "HTTP客户端遥测集成测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "http-client")
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "http-client-metrics")
  
  let counter = azimuth::Meter::create_counter(meter, "http.requests.total")
  let histogram = azimuth::Meter::create_histogram(meter, "http.request.duration")
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "http-client-logger")
  
  // 创建HTTP请求Span
  let http_span = azimuth::Tracer::start_span(tracer, "http.request")
  
  // 添加HTTP相关属性
  let http_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(http_attrs, "http.method", azimuth::StringValue("GET"))
  azimuth::Attributes::set(http_attrs, "http.url", azimuth::StringValue("https://api.example.com/data"))
  azimuth::Attributes::set(http_attrs, "http.scheme", azimuth::StringValue("https"))
  azimuth::Attributes::set(http_attrs, "http.host", azimuth::StringValue("api.example.com"))
  
  azimuth::Span::add_event(http_span, "http.request.started", Some(http_attrs))
  
  // 模拟HTTP请求
  let client = azimuth::HttpClient::new()
  let headers = [("Authorization", "Bearer token123")]
  let request = azimuth::HttpRequest::new("GET", "https://api.example.com/data", headers, None)
  
  // 更新度量
  azimuth::Counter::add(counter, 1.0)
  azimuth::Histogram::record(histogram, 150.5)  // 150.5ms
  
  // 模拟响应
  let response_headers = [("Content-Type", "application/json"), ("X-Response-Time", "150ms")]
  let response = azimuth::HttpResponse::new(200, response_headers, Some("{\"data\": \"value\"}"))
  
  // 添加响应属性
  let response_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(response_attrs, "http.status_code", azimuth::IntValue(200))
  azimuth::Attributes::set(response_attrs, "http.response_content_length", azimuth::IntValue(17))
  
  azimuth::Span::add_event(http_span, "http.response.received", Some(response_attrs))
  
  // 记录日志
  let log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("HTTP request completed successfully"),
    Some(response_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(http_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(http_span))),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(logger, log_record)
  
  // 结束Span
  azimuth::Span::set_status(http_span, azimuth::Ok)
  azimuth::Span::end(http_span)
  
  // 验证结果
  assert_eq(azimuth::HttpResponse::status_code(response), 200)
  assert_eq(counter.name, "http.requests.total")
  assert_eq(histogram.name, "http.request.duration")
}

// Test 7: HTTP client error handling
pub test "HTTP客户端错误处理测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "http-error-test")
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "http-error-metrics")
  
  let error_counter = azimuth::Meter::create_counter(meter, "http.errors.total")
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "http-error-logger")
  
  // 创建错误请求Span
  let error_span = azimuth::Tracer::start_span(tracer, "http.request.error")
  
  // 模拟网络错误
  let client = azimuth::HttpClient::new()
  let request = azimuth::HttpRequest::new("GET", "https://invalid.example.com/data", [], None)
  
  // 记录错误度量
  azimuth::Counter::add(error_counter, 1.0)
  
  // 添加错误事件
  let error_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(error_attrs, "error.type", azimuth::StringValue("NetworkError"))
  azimuth::Attributes::set(error_attrs, "error.message", azimuth::StringValue("Connection timeout"))
  azimuth::Attributes::set(error_attrs, "http.url", azimuth::StringValue("https://invalid.example.com/data"))
  
  azimuth::Span::add_event(error_span, "http.request.failed", Some(error_attrs))
  
  // 记录错误日志
  let error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("HTTP request failed: Connection timeout"),
    Some(error_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(error_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(error_span))),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(logger, error_log)
  
  // 设置错误状态并结束Span
  azimuth::Span::set_status(error_span, azimuth::Error)
  azimuth::Span::end(error_span)
  
  // 验证错误处理
  assert_eq(error_counter.name, "http.errors.total")
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
}

// Test 8: HTTP client concurrent requests
pub test "HTTP客户端并发请求测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "http-concurrent-test")
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "http-concurrent-metrics")
  
  let request_counter = azimuth::Meter::create_counter(meter, "http.concurrent.requests")
  let duration_histogram = azimuth::Meter::create_histogram(meter, "http.concurrent.duration")
  
  let client = azimuth::HttpClient::new()
  
  // 创建多个并发请求
  let spans = []
  for i in 1..11 {
    let span = azimuth::Tracer::start_span(tracer, "http.request.concurrent")
    
    // 添加请求属性
    let attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(attrs, "request.id", azimuth::StringValue("req-" + i.to_string()))
    azimuth::Attributes::set(attrs, "http.method", azimuth::StringValue("GET"))
    azimuth::Attributes::set(attrs, "http.url", azimuth::StringValue("https://api.example.com/data/" + i.to_string()))
    
    azimuth::Span::add_event(span, "request.started", Some(attrs))
    
    // 创建请求
    let headers = [("X-Request-ID", "req-" + i.to_string())]
    let request = azimuth::HttpRequest::new("GET", "https://api.example.com/data/" + i.to_string(), headers, None)
    
    // 更新度量
    azimuth::Counter::add(request_counter, 1.0)
    azimuth::Histogram::record(duration_histogram, i.to_double() * 10.0)  // 模拟不同延迟
    
    // 创建响应
    let response = azimuth::HttpResponse::new(200, [("Content-Type", "application/json")], Some("{\"id\": " + i.to_string() + "}"))
    
    // 添加响应事件
    let response_attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(response_attrs, "http.status_code", azimuth::IntValue(200))
    azimuth::Attributes::set(response_attrs, "request.id", azimuth::StringValue("req-" + i.to_string()))
    
    azimuth::Span::add_event(span, "response.received", Some(response_attrs))
    
    // 结束Span
    azimuth::Span::set_status(span, azimuth::Ok)
    azimuth::Span::end(span)
    
    spans.push(span)
  }
  
  // 验证所有请求都已处理
  assert_true(spans.length() == 10)
  assert_eq(request_counter.name, "http.concurrent.requests")
  assert_eq(duration_histogram.name, "http.concurrent.duration")
}