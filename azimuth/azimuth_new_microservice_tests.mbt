// 微服务架构遥测集成测试
test "微服务架构遥测集成测试" {
  // 模拟微服务架构中的多个服务
  let gateway_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "gateway-service")
  let auth_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "auth-service")
  let user_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "user-service")
  
  // 网关服务：接收外部请求
  let gateway_span_ctx = azimuth::SpanContext::new("trace-gateway-123", "span-gateway-root", true, "")
  let gateway_span = azimuth::Span::new("gateway.request", azimuth::Server, gateway_span_ctx)
  
  azimuth::Span::add_event(gateway_span, "gateway.request.received", Some([
    ("http.method", azimuth::StringValue("POST")),
    ("http.url", azimuth::StringValue("/api/orders")),
    ("user.agent", azimuth::StringValue("MobileApp/1.0")),
    ("client.ip", azimuth::StringValue("192.168.1.100"))
  ]))
  
  // 网关调用认证服务
  let auth_span_ctx = azimuth::SpanContext::new("trace-gateway-123", "span-auth-child", true, "")
  let auth_span = azimuth::Span::new("auth.validate", azimuth::Client, auth_span_ctx)
  
  azimuth::Span::add_event(auth_span, "auth.request.sent", Some([
    ("auth.token", azimuth::StringValue("jwt-token-123")),
    ("auth.method", azimuth::StringValue("oauth2"))
  ]))
  
  azimuth::Span::set_status(auth_span, azimuth::Ok)
  azimuth::Span::add_event(auth_span, "auth.response.received", Some([
    ("auth.user.id", azimuth::StringValue("user-456")),
    ("auth.permissions", azimuth::ArrayStringValue(["read", "write"]))
  ]))
  
  // 验证分布式追踪的一致性
  assert_eq(azimuth::SpanContext::trace_id(gateway_span_ctx), "trace-gateway-123")
  assert_eq(azimuth::SpanContext::trace_id(auth_span_ctx), "trace-gateway-123")
  
  // 验证Span ID的唯一性
  let span_ids = [
    azimuth::SpanContext::span_id(gateway_span_ctx),
    azimuth::SpanContext::span_id(auth_span_ctx)
  ]
  
  // 验证所有Span ID都是唯一的
  assert_true(span_ids[0] != span_ids[1])
  
  azimuth::Span::end(auth_span)
  azimuth::Span::end(gateway_span)
}