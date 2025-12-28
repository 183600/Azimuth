// 跨服务传播测试用例
// 测试遥测上下文在不同服务之间的传播和一致性

test "cross_service_trace_propagation" {
  // 模拟服务A创建初始追踪上下文
  let service_a_trace_id = "trace_001"
  let service_a_span_id = "span_a_001"
  let span_ctx_a = SpanContext::new(service_a_trace_id, service_a_span_id, true, "key1=value1")
  
  // 验证服务A的上下文
  assert_eq(SpanContext::trace_id(span_ctx_a), service_a_trace_id)
  assert_eq(SpanContext::span_id(span_ctx_a), service_a_span_id)
  assert_true(SpanContext::is_sampled(span_ctx_a))
  assert_true(SpanContext::is_valid(span_ctx_a))
  
  // 模拟通过CompositePropagator传播到服务B
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  let propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([propagator])
  
  // 注入追踪信息到carrier
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // 服务B从carrier中提取追踪信息
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // 验证提取的上下文
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

test "cross_service_baggage_propagation" {
  // 测试跨服务baggage传播
  let baggage = Baggage::new()
  
  // 在服务A中设置baggage条目
  let baggage_with_entries = Baggage::set_entry(baggage, "user.id", "user_12345")
  let baggage_final = Baggage::set_entry(baggage_with_entries, "request.id", "req_67890")
  
  // 验证baggage条目
  let user_id = Baggage::get_entry(baggage_final, "user.id")
  let request_id = Baggage::get_entry(baggage_final, "request.id")
  let missing_entry = Baggage::get_entry(baggage_final, "nonexistent")
  
  assert_eq(user_id, Some("user_12345"))
  assert_eq(request_id, Some("req_67890"))
  assert_eq(missing_entry, None)
  
  // 测试baggage条目移除
  let baggage_after_removal = Baggage::remove_entry(baggage_final, "user.id")
  let removed_user_id = Baggage::get_entry(baggage_after_removal, "user.id")
  let still_present_request_id = Baggage::get_entry(baggage_after_removal, "request.id")
  
  assert_eq(removed_user_id, None)
  assert_eq(still_present_request_id, Some("req_67890"))
}

test "cross_service_context_chain_propagation" {
  // 测试跨服务上下文链传播
  let root_ctx = Context::root()
  
  // 服务A添加上下文信息
  let service_a_key = ContextKey::new("service.a")
  let ctx_after_a = Context::with_value(root_ctx, service_a_key, "processed_by_a")
  
  // 服务B添加上下文信息
  let service_b_key = ContextKey::new("service.b")
  let ctx_after_b = Context::with_value(ctx_after_a, service_b_key, "processed_by_b")
  
  // 服务C添加上下文信息
  let service_c_key = ContextKey::new("service.c")
  let final_ctx = Context::with_value(ctx_after_b, service_c_key, "processed_by_c")
  
  // 验证所有服务的上下文信息都存在
  let service_a_value = Context::get(final_ctx, service_a_key)
  let service_b_value = Context::get(final_ctx, service_b_key)
  let service_c_value = Context::get(final_ctx, service_c_key)
  
  assert_eq(service_a_value, Some("processed_by_a"))
  assert_eq(service_b_value, Some("processed_by_b"))
  assert_eq(service_c_value, Some("processed_by_c"))
}

test "cross_service_header_propagation" {
  // 测试跨服务HTTP头部传播
  let carrier = TextMapCarrier::new()
  
  // 设置追踪相关的头部
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  TextMapCarrier::set(carrier, "baggage", "userId=amy,serverNode=AFRICA")
  
  // 验证头部设置
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  let tracestate = TextMapCarrier::get(carrier, "tracestate")
  let baggage = TextMapCarrier::get(carrier, "baggage")
  let missing_header = TextMapCarrier::get(carrier, "x-missing-header")
  
  assert_eq(traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(tracestate, Some("rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"))
  assert_eq(baggage, Some("userId=amy,serverNode=AFRICA"))
  assert_eq(missing_header, None)
}

test "cross_service_telemetry_consistency" {
  // 测试跨服务遥测数据一致性
  let trace_id = "consistency_test_trace"
  let parent_span_id = "parent_span_001"
  let child_span_id = "child_span_002"
  
  // 创建父span上下文
  let parent_span_ctx = SpanContext::new(trace_id, parent_span_id, true, "")
  
  // 创建子span上下文（应该继承父span的trace_id）
  let child_span_ctx = SpanContext::new(trace_id, child_span_id, true, "")
  
  // 验证trace_id一致性
  assert_eq(SpanContext::trace_id(parent_span_ctx), SpanContext::trace_id(child_span_ctx))
  
  // 验证span_id不同
  assert_not_eq(SpanContext::span_id(parent_span_ctx), SpanContext::span_id(child_span_ctx))
  
  // 验证采样标志一致性
  assert_eq(SpanContext::is_sampled(parent_span_ctx), SpanContext::is_sampled(child_span_ctx))
  
  // 验证两个上下文都有效
  assert_true(SpanContext::is_valid(parent_span_ctx))
  assert_true(SpanContext::is_valid(child_span_ctx))
}