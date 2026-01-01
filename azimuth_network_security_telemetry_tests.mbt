// 网络安全遥测测试
import "azimuth/azimuth"

pub test "网络安全遥测测试" {
  // 测试安全相关的Span属性
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "security-tracer")
  
  let span = azimuth::Tracer::start_span(tracer, "secure-operation")
  
  // 添加安全相关属性
  azimuth::Span::add_event(span, "security.auth", Some([
    ("user.id", azimuth::StringValue("user-123")),
    ("auth.method", azimuth::StringValue("oauth2")),
    ("auth.success", azimuth::BoolValue(true)),
    ("security.level", azimuth::StringValue("high"))
  ]))
  
  azimuth::Span::add_event(span, "security.authorization", Some([
    ("permission.granted", azimuth::BoolValue(true)),
    ("resource.accessed", azimuth::StringValue("/api/secure/data")),
    ("access.timestamp", azimuth::StringValue("2025-01-01T12:00:00Z"))
  ]))
  
  // 测试安全相关的度量
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "security-meter")
  
  let auth_counter = azimuth::Meter::create_counter(meter, "security.auth.attempts")
  let auth_success_counter = azimuth::Meter::create_counter(meter, "security.auth.success")
  let auth_failure_counter = azimuth::Meter::create_counter(meter, "security.auth.failures")
  let security_events_counter = azimuth::Meter::create_counter(meter, "security.events")
  
  // 记录安全度量
  azimuth::Counter::add(auth_counter, 100.0)
  azimuth::Counter::add(auth_success_counter, 85.0)
  azimuth::Counter::add(auth_failure_counter, 15.0)
  azimuth::Counter::add(security_events_counter, 25.0)
  
  // 测试安全响应时间
  let security_response_time = azimuth::Meter::create_histogram(meter, "security.response.time", Some("Security operation response time"), Some("ms"))
  
  for i in 0..50 {
    let response_time = 100.0 + (i.to_double() * 2.0)  // 模拟不同的响应时间
    azimuth::Histogram::record(security_response_time, response_time)
  }
  
  // 测试安全相关日志
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "security-logger")
  
  // 安全事件日志
  let security_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Security event detected"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(span))),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(logger, security_log)
  
  // 测试网络安全相关属性
  let security_attrs = azimuth::Attributes::new()
  
  azimuth::Attributes::set(security_attrs, "network.protocol", azimuth::StringValue("HTTPS"))
  azimuth::Attributes::set(security_attrs, "network.tls.version", azimuth::StringValue("TLSv1.3"))
  azimuth::Attributes::set(security_attrs, "network.cipher.suite", azimuth::StringValue("TLS_AES_256_GCM_SHA384"))
  azimuth::Attributes::set(security_attrs, "client.ip", azimuth::StringValue("192.168.1.100"))
  azimuth::Attributes::set(security_attrs, "client.user.agent", azimuth::StringValue("SecureClient/1.0"))
  azimuth::Attributes::set(security_attrs, "security.threat.level", azimuth::StringValue("low"))
  azimuth::Attributes::set(security_attrs, "security.anomaly.detected", azimuth::BoolValue(false))
  
  // 测试HTTP安全头
  let http_security_attrs = azimuth::Attributes::new()
  
  azimuth::Attributes::set(http_security_attrs, "http.security.strict.transport.security", azimuth::StringValue("max-age=31536000; includeSubDomains"))
  azimuth::Attributes::set(http_security_attrs, "http.security.content.type.options", azimuth::StringValue("nosniff"))
  azimuth::Attributes::set(http_security_attrs, "http.security.x.frame.options", azimuth::StringValue("DENY"))
  azimuth::Attributes::set(http_security_attrs, "http.security.x.content.type.options", azimuth::StringValue("nosniff"))
  azimuth::Attributes::set(http_security_attrs, "http.security.referrer.policy", azimuth::StringValue("strict-origin-when-cross-origin"))
  
  // 测试API安全相关度量
  let api_security_counter = azimuth::Meter::create_counter(meter, "api.security.requests")
  let api_rate_limit_counter = azimuth::Meter::create_counter(meter, "api.security.rate.limited")
  let api_blocked_counter = azimuth::Meter::create_counter(meter, "api.security.blocked")
  
  // 记录API安全度量
  azimuth::Counter::add(api_security_counter, 1000.0)
  azimuth::Counter::add(api_rate_limit_counter, 50.0)
  azimuth::Counter::add(api_blocked_counter, 5.0)
  
  // 测试安全相关Resource属性
  let security_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("security-service")),
    ("service.version", azimuth::StringValue("2.1.0")),
    ("security.compliance.level", azimuth::StringValue("SOC2")),
    ("security.certification", azimuth::StringValue("ISO27001")),
    ("data.encryption.at.rest", azimuth::BoolValue(true)),
    ("data.encryption.in.transit", azimuth::BoolValue(true)),
    ("security.audit.enabled", azimuth::BoolValue(true))
  ])
  
  // 验证安全Resource属性
  assert_eq(azimuth::Resource::get_attribute(security_resource, "service.name"), Some(azimuth::StringValue("security-service")))
  assert_eq(azimuth::Resource::get_attribute(security_resource, "security.compliance.level"), Some(azimuth::StringValue("SOC2")))
  assert_eq(azimuth::Resource::get_attribute(security_resource, "data.encryption.at.rest"), Some(azimuth::BoolValue(true)))
  
  // 测试安全相关的Baggage传播
  let security_baggage = azimuth::Baggage::new()
  let security_context_baggage = azimuth::Baggage::set_entry(security_baggage, "security.context.token", "secure-token-123")
  let user_context_baggage = azimuth::Baggage::set_entry(security_context_baggage, "user.security.level", "privileged")
  let session_context_baggage = azimuth::Baggage::set_entry(user_context_baggage, "session.security.id", "session-456")
  
  // 验证安全Baggage传播
  assert_eq(azimuth::Baggage::get_entry(session_context_baggage, "security.context.token"), Some("secure-token-123"))
  assert_eq(azimuth::Baggage::get_entry(session_context_baggage, "user.security.level"), Some("privileged"))
  assert_eq(azimuth::Baggage::get_entry(session_context_baggage, "session.security.id"), Some("session-456"))
  
  // 测试安全相关的Propagator
  let security_trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let security_baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let security_propagators = [security_trace_propagator, security_baggage_propagator]
  let security_composite_propagator = azimuth::CompositePropagator::new(security_propagators)
  
  let security_carrier = azimuth::TextMapCarrier::new()
  let security_ctx = azimuth::Context::root()
  
  // 注入安全上下文
  azimuth::CompositePropagator::inject(security_composite_propagator, security_ctx, security_carrier)
  
  // 验证安全头被正确注入
  let trace_header = azimuth::TextMapCarrier::get(security_carrier, "traceparent")
  assert_eq(trace_header, Some("00-test-trace-id-test-span-id-01"))
  
  // 测试安全相关的HTTP客户端操作
  let secure_client = azimuth::HttpClient::new()
  
  let secure_headers = [
    ("Authorization", azimuth::StringValue("Bearer secure-jwt-token")),
    ("X-API-Key", azimuth::StringValue("secure-api-key-123")),
    ("X-Request-ID", azimuth::StringValue("secure-request-456")),
    ("X-Security-Token", azimuth::StringValue("security-token-789"))
  ]
  
  let secure_request = azimuth::HttpRequest::new("POST", "https://secure-api.example.com/auth", secure_headers, Some("secure payload"))
  
  // 验证安全请求
  assert_eq(azimuth::HttpRequest::http_method(secure_request), "POST")
  assert_eq(azimuth::HttpRequest::url(secure_request), "https://secure-api.example.com/auth")
  
  // 测试安全响应处理
  let secure_response_headers = [
    ("X-Security-Response-ID", azimuth::StringValue("secure-response-123")),
    ("X-Rate-Limit-Remaining", azimuth::StringValue("4999")),
    ("X-Security-Policy", azimuth::StringValue("strict"))
  ]
  
  let secure_response = azimuth::HttpResponse::new(200, secure_response_headers, Some("{\"status\": \"authenticated\", \"level\": \"high\"}"))
  
  // 验证安全响应
  assert_eq(azimuth::HttpResponse::status_code(secure_response), 200)
  
  azimuth::Span::end(span)
}