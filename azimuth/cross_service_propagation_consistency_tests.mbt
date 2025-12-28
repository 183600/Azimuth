// 跨服务传播一致性测试用例
// 测试跨服务调用时上下文传播的一致性和正确性

test "cross_service_trace_context_propagation" {
  // 测试跨服务跟踪上下文传播
  let original_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let original_span_id = "b7ad6b7169203331"
  let span_context = SpanContext::new(original_trace_id, original_span_id, true, "")
  
  // 创建传播器
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  // 创建载体
  let carrier = TextMapCarrier::new()
  let root_ctx = Context::root()
  
  // 注入上下文
  CompositePropagator::inject(composite, root_ctx, carrier)
  
  // 验证注入的上下文
  let injected_trace = TextMapCarrier::get(carrier, "traceparent")
  match injected_trace {
    Some(trace_value) => {
      assert_true(trace_value.contains(original_trace_id))
      assert_true(trace_value.contains(original_span_id))
    }
    _ => assert_false(true, "Expected traceparent header")
  }
  
  // 提取上下文
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  match extracted_value {
    Some(value) => assert_eq(value, "true")
    _ => assert_false(true, "Expected extracted context value")
  }
}

test "cross_service_baggage_propagation" {
  // 测试跨服务baggage传播
  let baggage = Baggage::new()
  let baggage_with_entries = Baggage::set_entry(baggage, "user.id", "12345")
  let final_baggage = Baggage::set_entry(baggage_with_entries, "request.id", "req-67890")
  
  // 验证baggage条目
  let user_id = Baggage::get_entry(final_baggage, "user.id")
  match user_id {
    Some(id) => assert_eq(id, "12345")
    _ => assert_false(true, "Expected user.id baggage entry")
  }
  
  let request_id = Baggage::get_entry(final_baggage, "request.id")
  match request_id {
    Some(id) => assert_eq(id, "req-67890")
    _ => assert_false(true, "Expected request.id baggage entry")
  }
  
  // 测试baggage条目移除
  let baggage_without_user = Baggage::remove_entry(final_baggage, "user.id")
  let removed_user_id = Baggage::get_entry(baggage_without_user, "user.id")
  match removed_user_id {
    Some(_) => assert_false(true, "User.id should be removed")
    _ => ()  // 期望为None
  }
  
  // 验证其他条目仍然存在
  let remaining_request_id = Baggage::get_entry(baggage_without_user, "request.id")
  match remaining_request_id {
    Some(id) => assert_eq(id, "req-67890")
    _ => assert_false(true, "Request.id should still exist")
  }
}

test "cross_service_context_chain_propagation" {
  // 测试跨服务上下文链传播
  let root_ctx = Context::root()
  let service1_key = ContextKey::new("service1")
  let service2_key = ContextKey::new("service2")
  let service3_key = ContextKey::new("service3")
  
  // 模拟服务链中的上下文传递
  let ctx_after_service1 = Context::with_value(root_ctx, service1_key, "processed-by-service1")
  let ctx_after_service2 = Context::with_value(ctx_after_service1, service2_key, "processed-by-service2")
  let ctx_after_service3 = Context::with_value(ctx_after_service2, service3_key, "processed-by-service3")
  
  // 验证每个服务的上下文都得到保持
  let service1_value = Context::get(ctx_after_service3, service1_key)
  match service1_value {
    Some(value) => assert_eq(value, "processed-by-service1")
    _ => assert_false(true, "Expected service1 context")
  }
  
  let service2_value = Context::get(ctx_after_service3, service2_key)
  match service2_value {
    Some(value) => assert_eq(value, "processed-by-service2")
    _ => assert_false(true, "Expected service2 context")
  }
  
  let service3_value = Context::get(ctx_after_service3, service3_key)
  match service3_value {
    Some(value) => assert_eq(value, "processed-by-service3")
    _ => assert_false(true, "Expected service3 context")
  }
}

test "cross_service_header_propagation_consistency" {
  // 测试跨服务头部传播一致性
  let carrier = TextMapCarrier::new()
  
  // 模拟设置多个跟踪相关的头部
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  TextMapCarrier::set(carrier, "baggage", "userId=alice,serverNode=AF:2")
  TextMapCarrier::set(carrier, "x-request-id", "req-12345")
  
  // 验证头部可以正确获取
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  match traceparent {
    Some(value) => {
      assert_true(value.contains("0af7651916cd43dd8448eb211c80319c"))
      assert_true(value.contains("b7ad6b7169203331"))
    }
    _ => assert_false(true, "Expected traceparent header")
  }
  
  let tracestate = TextMapCarrier::get(carrier, "tracestate")
  match tracestate {
    Some(value) => {
      assert_true(value.contains("rojo=00f067aa0ba902b7"))
      assert_true(value.contains("congo=t61rcWkgMzE"))
    }
    _ => assert_false(true, "Expected tracestate header")
  }
  
  let baggage = TextMapCarrier::get(carrier, "baggage")
  match baggage {
    Some(value) => {
      assert_true(value.contains("userId=alice"))
      assert_true(value.contains("serverNode=AF:2"))
    }
    _ => assert_false(true, "Expected baggage header")
  }
  
  let request_id = TextMapCarrier::get(carrier, "x-request-id")
  match request_id {
    Some(value) => assert_eq(value, "req-12345")
    _ => assert_false(true, "Expected x-request-id header")
  }
}

test "cross_service_sampling_consistency" {
  // 测试跨服务采样一致性
  let sampled_context = SpanContext::new("trace123", "span456", true, "sampled=true")
  let not_sampled_context = SpanContext::new("trace789", "span012", false, "")
  
  // 验证采样状态保持
  assert_true(SpanContext::is_sampled(sampled_context))
  assert_false(SpanContext::is_sampled(not_sampled_context))
  
  // 验证跟踪ID和Span ID的有效性
  assert_true(SpanContext::is_valid(sampled_context))
  assert_true(SpanContext::is_valid(not_sampled_context))
  
  // 测试无效上下文
  let invalid_trace_context = SpanContext::new("", "span456", true, "")
  let invalid_span_context = SpanContext::new("trace123", "", true, "")
  
  assert_false(SpanContext::is_valid(invalid_trace_context))
  assert_false(SpanContext::is_valid(invalid_span_context))
}

test "cross_service_propagator_composition" {
  // 测试传播器组合
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // 创建组合传播器
  let composite = CompositePropagator::new([trace_propagator])
  
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // 测试注入
  CompositePropagator::inject(composite, ctx, carrier)
  
  // 测试提取
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  match extracted_value {
    Some(value) => assert_eq(value, "true")
    _ => assert_false(true, "Expected extracted context")
  }
}

test "cross_service_error_propagation" {
  // 测试跨服务错误传播
  let error_ctx = Context::with_value(Context::root(), ContextKey::new("error.code"), "500")
  let error_with_message = Context::with_value(error_ctx, ContextKey::new("error.message"), "Internal Server Error")
  
  // 验证错误信息传播
  let error_code = Context::get(error_with_message, ContextKey::new("error.code"))
  match error_code {
    Some(code) => assert_eq(code, "500")
    _ => assert_false(true, "Expected error code")
  }
  
  let error_message = Context::get(error_with_message, ContextKey::new("error.message"))
  match error_message {
    Some(message) => assert_eq(message, "Internal Server Error")
    _ => assert_false(true, "Expected error message")
  }
  
  // 测试错误恢复上下文
  let recovery_ctx = Context::with_value(error_with_message, ContextKey::new("recovery.attempt"), "1")
  let recovery_attempt = Context::get(recovery_ctx, ContextKey::new("recovery.attempt"))
  match recovery_attempt {
    Some(attempt) => assert_eq(attempt, "1")
    _ => assert_false(true, "Expected recovery attempt")
  }
  
  // 验证原始错误信息仍然存在
  let original_error_code = Context::get(recovery_ctx, ContextKey::new("error.code"))
  match original_error_code {
    Some(code) => assert_eq(code, "500")
    _ => assert_false(true, "Expected original error code")
  }
}

test "cross_service_metadata_propagation" {
  // 测试跨服务元数据传播
  let metadata_ctx = Context::with_value(Context::root(), ContextKey::new("service.version"), "1.2.3")
  let metadata_with_env = Context::with_value(metadata_ctx, ContextKey::new("deployment.environment"), "production")
  let metadata_with_region = Context::with_value(metadata_with_env, ContextKey::new("deployment.region"), "us-west-2")
  let final_metadata = Context::with_value(metadata_with_region, ContextKey::new("request.timeout"), "30s")
  
  // 验证所有元数据都得到传播
  let service_version = Context::get(final_metadata, ContextKey::new("service.version"))
  match service_version {
    Some(version) => assert_eq(version, "1.2.3")
    _ => assert_false(true, "Expected service version")
  }
  
  let environment = Context::get(final_metadata, ContextKey::new("deployment.environment"))
  match environment {
    Some(env) => assert_eq(env, "production")
    _ => assert_false(true, "Expected deployment environment")
  }
  
  let region = Context::get(final_metadata, ContextKey::new("deployment.region"))
  match region {
    Some(r) => assert_eq(r, "us-west-2")
    _ => assert_false(true, "Expected deployment region")
  }
  
  let timeout = Context::get(final_metadata, ContextKey::new("request.timeout"))
  match timeout {
    Some(t) => assert_eq(t, "30s")
    _ => assert_false(true, "Expected request timeout")
  }
}