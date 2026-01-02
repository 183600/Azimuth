// Cross-Service Propagation Tests
// 验证遥测上下文在跨服务调用中的正确传播

test "trace context propagation across services" {
  // 服务A：创建初始span
  let trace_id = "trace-abc123"
  let span_a_id = "span-a001"
  let span_ctx_a = azimuth::SpanContext::new(trace_id, span_a_id, true, "service=A,env=prod")
  let span_a = azimuth::Span::new("service-a-operation", azimuth::Server, span_ctx_a)
  
  // 模拟HTTP传播载体
  let carrier = azimuth::TextMapCarrier::new()
  
  // 使用W3C Trace Context传播器注入上下文
  let propagator = azimuth::W3CTraceContextPropagator::new()
  azimuth::W3CTraceContextPropagator::inject(propagator, azimuth::Span::context(span_a), carrier)
  
  // 验证注入的头部
  let headers = azimuth::TextMapCarrier::headers(carrier)
  assert_true(headers.length > 0)
  
  // 查找traceparent头部
  let traceparent_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  assert_true(traceparent_header.length > 0)
  
  // 服务B：提取上下文
  let span_ctx_b = azimuth::W3CTraceContextPropagator::extract(propagator, carrier)
  
  // 验证上下文传播的一致性
  assert_eq(azimuth::SpanContext::trace_id(span_ctx_b), trace_id)
  assert_true(azimuth::SpanContext::is_sampled(span_ctx_b))
  assert_not_eq(azimuth::SpanContext::span_id(span_ctx_b), span_a_id) // 应该是新的span ID
  
  // 创建子span
  let span_b = azimuth::Span::new("service-b-operation", azimuth::Server, span_ctx_b)
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::context(span_b)), trace_id)
}

test "baggage propagation across service boundaries" {
  // 服务A：设置baggage
  let baggage = azimuth::Baggage::new()
  azimuth::Baggage::set_entry(baggage, "user.id", "12345")
  azimuth::Baggage::set_entry(baggage, "request.id", "req-67890")
  azimuth::Baggage::set_entry(baggage, "tenant.id", "tenant-001")
  
  // 创建传播载体
  let carrier = azimuth::TextMapCarrier::new()
  
  // 使用W3C Baggage传播器注入
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  azimuth::W3CBaggagePropagator::inject(baggage_propagator, baggage, carrier)
  
  // 验证baggage头部
  let baggage_header = azimuth::TextMapCarrier::get(carrier, "baggage")
  assert_true(baggage_header.length > 0)
  
  // 服务B：提取baggage
  let extracted_baggage = azimuth::W3CBaggagePropagator::extract(baggage_propagator, carrier)
  
  // 验证baggage条目传播的一致性
  let user_id = azimuth::Baggage::get_entry(extracted_baggage, "user.id")
  let request_id = azimuth::Baggage::get_entry(extracted_baggage, "request.id")
  let tenant_id = azimuth::Baggage::get_entry(extracted_baggage, "tenant.id")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(request_id, Some("req-67890"))
  assert_eq(tenant_id, Some("tenant-001"))
}

test "composite propagator with multiple contexts" {
  // 创建复合传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let composite_propagator = azimuth::CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 创建span上下文和baggage
  let span_ctx = azimuth::SpanContext::new("trace-xyz789", "span-x123", true, "key=value")
  let baggage = azimuth::Baggage::new()
  azimuth::Baggage::set_entry(baggage, "correlation.id", "corr-456")
  azimuth::Baggage::set_entry(baggage, "session.id", "sess-789")
  
  // 创建传播载体
  let carrier = azimuth::TextMapCarrier::new()
  
  // 使用复合传播器注入
  azimuth::CompositePropagator::inject(composite_propagator, span_ctx, carrier)
  azimuth::CompositePropagator::inject_baggage(composite_propagator, baggage, carrier)
  
  // 验证多个头部存在
  let headers = azimuth::TextMapCarrier::headers(carrier)
  let has_traceparent = false
  let has_baggage = false
  
  for header in headers {
    if (header.0 == "traceparent") {
      has_traceparent = true
    }
    if (header.0 == "baggage") {
      has_baggage = true
    }
  }
  
  assert_true(has_traceparent)
  assert_true(has_baggage)
  
  // 提取并验证
  let extracted_span_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  let extracted_baggage = azimuth::CompositePropagator::extract_baggage(composite_propagator, carrier)
  
  assert_eq(azimuth::SpanContext::trace_id(extracted_span_ctx), "trace-xyz789")
  assert_eq(azimuth::Baggage::get_entry(extracted_baggage, "correlation.id"), Some("corr-456"))
  assert_eq(azimuth::Baggage::get_entry(extracted_baggage, "session.id"), Some("sess-789"))
}

test "context propagation with special characters" {
  // 测试包含特殊字符的上下文传播
  let span_ctx = azimuth::SpanContext::new("trace-special-123", "span-special-456", true, "key=value%20with%20spaces")
  let baggage = azimuth::Baggage::new()
  
  // 设置包含特殊字符的baggage条目
  azimuth::Baggage::set_entry(baggage, "user.email", "test@example.com")
  azimuth::Baggage::set_entry(baggage, "complex.value", "a=b;c=d;e=f")
  azimuth::Baggage::set_entry(baggage, "unicode.value", "测试值")
  azimuth::Baggage::set_entry(baggage, "space.value", "value with spaces")
  
  let carrier = azimuth::TextMapCarrier::new()
  let composite_propagator = azimuth::CompositePropagator::new([
    azimuth::W3CTraceContextPropagator::new(),
    azimuth::W3CBaggagePropagator::new()
  ])
  
  // 注入
  azimuth::CompositePropagator::inject(composite_propagator, span_ctx, carrier)
  azimuth::CompositePropagator::inject_baggage(composite_propagator, baggage, carrier)
  
  // 提取
  let extracted_span_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  let extracted_baggage = azimuth::CompositePropagator::extract_baggage(composite_propagator, carrier)
  
  // 验证特殊字符处理正确
  assert_eq(azimuth::SpanContext::trace_id(extracted_span_ctx), "trace-special-123")
  assert_eq(azimuth::Baggage::get_entry(extracted_baggage, "user.email"), Some("test@example.com"))
  assert_eq(azimuth::Baggage::get_entry(extracted_baggage, "unicode.value"), Some("测试值"))
}