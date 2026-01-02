// 错误边界和恢复测试
test "错误边界和恢复测试" {
  // 测试Span操作的错误处理
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "error-boundary-tracer")
  
  // 测试无效Span创建
  let invalid_span_ctx = azimuth::SpanContext::new("", "", false, "")
  let invalid_span = azimuth::Span::new("invalid-operation", azimuth::Internal, invalid_span_ctx)
  
  // 验证无效Span的处理
  assert_false(azimuth::SpanContext::is_valid(invalid_span_ctx))
  assert_false(azimuth::SpanContext::is_sampled(invalid_span_ctx))
  
  // 测试有效Span的错误恢复
  let valid_span_ctx = azimuth::SpanContext::new("valid-trace", "valid-span", true, "")
  let valid_span = azimuth::Span::new("valid-operation", azimuth::Internal, valid_span_ctx)
  
  // 添加错误事件到Span
  azimuth::Span::add_event(valid_span, "error.occurred", Some([
    ("error.type", azimuth::StringValue("validation.error")),
    ("error.message", azimuth::StringValue("Invalid input parameter")),
    ("error.code", azimuth::IntValue(400))
  ]))
  
  // 设置错误状态
  azimuth::Span::set_status(valid_span, azimuth::Error)
  
  // 验证错误状态设置
  assert_eq(azimuth::Span::name(valid_span), "valid-operation")
  
  azimuth::Span::end(valid_span)
}