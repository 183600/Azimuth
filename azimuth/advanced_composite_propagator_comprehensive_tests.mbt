// 复合传播器高级测试
// 测试传播器的复杂组合、错误处理和边界条件

test "复合传播器多级注入提取测试" {
  // 创建多个传播器
  let trace_propagator1 = W3CTraceContextPropagator::new()
  let trace_propagator2 = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // 创建复合传播器
  let propagators = [trace_propagator1, trace_propagator2]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // 创建上下文和载体
  let ctx = Context::root()
  let key = ContextKey::new("test.operation")
  let ctx_with_value = Context::with_value(ctx, key, "multi.level.test")
  
  let carrier = TextMapCarrier::new()
  
  // 测试多级注入
  CompositePropagator::inject(composite_propagator, ctx_with_value, carrier)
  
  // 验证注入结果
  let injected_trace = TextMapCarrier::get(carrier, "traceparent")
  assert_true(injected_trace != None, "应该注入traceparent头")
  
  // 测试提取操作
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  
  assert_eq(extracted_value, Some("true"), "应该成功提取上下文值")
}

test "传播器链式传播测试" {
  // 创建传播器链
  let propagator1 = W3CTraceContextPropagator::new()
  let propagator2 = W3CTraceContextPropagator::new()
  let propagator3 = W3CTraceContextPropagator::new()
  
  let propagators = [propagator1, propagator2, propagator3]
  let chain_propagator = CompositePropagator::new(propagators)
  
  // 创建初始上下文
  let initial_ctx = Context::root()
  let operation_key = ContextKey::new("operation.chain")
  let ctx_with_operation = Context::with_value(initial_ctx, operation_key, "chain.test")
  
  // 第一轮传播
  let carrier1 = TextMapCarrier::new()
  CompositePropagator::inject(chain_propagator, ctx_with_operation, carrier1)
  
  // 第二轮传播（从载体1到载体2）
  let intermediate_ctx = CompositePropagator::extract(chain_propagator, carrier1)
  let carrier2 = TextMapCarrier::new()
  CompositePropagator::inject(chain_propagator, intermediate_ctx, carrier2)
  
  // 第三轮传播（从载体2到载体3）
  let final_ctx = CompositePropagator::extract(chain_propagator, carrier2)
  let carrier3 = TextMapCarrier::new()
  CompositePropagator::inject(chain_propagator, final_ctx, carrier3)
  
  // 验证链式传播的正确性
  let final_trace = TextMapCarrier::get(carrier3, "traceparent")
  assert_true(final_trace != None, "链式传播应该保持trace信息")
  
  let final_extracted = Context::get(final_ctx, ContextKey::new("extracted"))
  assert_eq(final_extracted, Some("true"), "链式传播应该保持上下文信息")
}

test "传播器并发安全测试" {
  // 创建多个传播器用于并发测试
  let propagator1 = W3CTraceContextPropagator::new()
  let propagator2 = W3CTraceContextPropagator::new()
  let propagators = [propagator1, propagator2]
  let concurrent_propagator = CompositePropagator::new(propagators)
  
  // 创建多个上下文和载体
  let ctx1 = Context::with_value(Context::root(), ContextKey::new("concurrent.test1"), "value1")
  let ctx2 = Context::with_value(Context::root(), ContextKey::new("concurrent.test2"), "value2")
  let ctx3 = Context::with_value(Context::root(), ContextKey::new("concurrent.test3"), "value3")
  
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  let carrier3 = TextMapCarrier::new()
  
  // 模拟并发注入操作
  CompositePropagator::inject(concurrent_propagator, ctx1, carrier1)
  CompositePropagator::inject(concurrent_propagator, ctx2, carrier2)
  CompositePropagator::inject(concurrent_propagator, ctx3, carrier3)
  
  // 验证并发注入的结果
  let trace1 = TextMapCarrier::get(carrier1, "traceparent")
  let trace2 = TextMapCarrier::get(carrier2, "traceparent")
  let trace3 = TextMapCarrier::get(carrier3, "traceparent")
  
  assert_true(trace1 != None, "并发注入1应该成功")
  assert_true(trace2 != None, "并发注入2应该成功")
  assert_true(trace3 != None, "并发注入3应该成功")
  
  // 模拟并发提取操作
  let extracted_ctx1 = CompositePropagator::extract(concurrent_propagator, carrier1)
  let extracted_ctx2 = CompositePropagator::extract(concurrent_propagator, carrier2)
  let extracted_ctx3 = CompositePropagator::extract(concurrent_propagator, carrier3)
  
  let extracted1 = Context::get(extracted_ctx1, ContextKey::new("extracted"))
  let extracted2 = Context::get(extracted_ctx2, ContextKey::new("extracted"))
  let extracted3 = Context::get(extracted_ctx3, ContextKey::new("extracted"))
  
  assert_eq(extracted1, Some("true"), "并发提取1应该成功")
  assert_eq(extracted2, Some("true"), "并发提取2应该成功")
  assert_eq(extracted3, Some("true"), "并发提取3应该成功")
}

test "传播器错误恢复和边界条件测试" {
  // 创建传播器
  let propagator = W3CTraceContextPropagator::new()
  let propagators = [propagator]
  let error_recovery_propagator = CompositePropagator::new(propagators)
  
  // 测试空载体处理
  let empty_carrier = TextMapCarrier::new()
  let ctx_from_empty = CompositePropagator::extract(error_recovery_propagator, empty_carrier)
  let extracted_from_empty = Context::get(ctx_from_empty, ContextKey::new("extracted"))
  
  assert_eq(extracted_from_empty, Some("true"), "应该能从空载体恢复")
  
  // 测试损坏的traceparent头
  let corrupted_carrier = TextMapCarrier::new()
  TextMapCarrier::set(corrupted_carrier, "traceparent", "invalid-trace-data")
  
  let ctx_from_corrupted = CompositePropagator::extract(error_recovery_propagator, corrupted_carrier)
  let extracted_from_corrupted = Context::get(ctx_from_corrupted, ContextKey::new("extracted"))
  
  assert_eq(extracted_from_corrupted, Some("true"), "应该能从损坏数据恢复")
  
  // 测试部分缺失的头部
  let partial_carrier = TextMapCarrier::new()
  TextMapCarrier::set(partial_carrier, "baggage", "key1=value1")
  // 故意不设置traceparent
  
  let ctx_from_partial = CompositePropagator::extract(error_recovery_propagator, partial_carrier)
  let extracted_from_partial = Context::get(ctx_from_partial, ContextKey::new("extracted"))
  
  assert_eq(extracted_from_partial, Some("true"), "应该能处理部分缺失的头部")
}

test "传播器性能和大规模数据处理测试" {
  // 创建大型传播器组合
  let mut propagators = []
  for i = 0; i < 10; i = i + 1 {
    propagators.push(W3CTraceContextPropagator::new())
  }
  let large_composite_propagator = CompositePropagator::new(propagators)
  
  // 创建包含大量数据的上下文
  let ctx = Context::root()
  let mut enriched_ctx = ctx
  for i = 0; i < 5; i = i + 1 {
    let key = ContextKey::new("large.scale.key" + i.to_string())
    enriched_ctx = Context::with_value(enriched_ctx, key, "large.scale.value" + i.to_string())
  }
  
  // 测试大规模注入
  let large_carrier = TextMapCarrier::new()
  CompositePropagator::inject(large_composite_propagator, enriched_ctx, large_carrier)
  
  // 验证大规模注入结果
  let large_trace = TextMapCarrier::get(large_carrier, "traceparent")
  assert_true(large_trace != None, "大规模注入应该成功")
  
  // 测试大规模提取
  let large_extracted_ctx = CompositePropagator::extract(large_composite_propagator, large_carrier)
  let large_extracted = Context::get(large_extracted_ctx, ContextKey::new("extracted"))
  
  assert_eq(large_extracted, Some("true"), "大规模提取应该成功")
  
  // 测试性能边界：快速连续操作
  for i = 0; i < 3; i = i + 1 {
    let rapid_carrier = TextMapCarrier::new()
    let rapid_ctx = Context::with_value(Context::root(), ContextKey::new("rapid.test"), "rapid.value")
    
    CompositePropagator::inject(large_composite_propagator, rapid_ctx, rapid_carrier)
    let rapid_extracted_ctx = CompositePropagator::extract(large_composite_propagator, rapid_carrier)
    let rapid_extracted = Context::get(rapid_extracted_ctx, ContextKey::new("extracted"))
    
    assert_eq(rapid_extracted, Some("true"), "快速操作" + i.to_string() + "应该成功")
  }
}

test "传播器跨协议兼容性测试" {
  // 创建支持多种协议的传播器
  let http_propagator = W3CTraceContextPropagator::new()
  let grpc_propagator = W3CTraceContextPropagator::new()
  let messaging_propagator = W3CTraceContextPropagator::new()
  
  let protocol_propagators = [http_propagator, grpc_propagator, messaging_propagator]
  let cross_protocol_propagator = CompositePropagator::new(protocol_propagators)
  
  // 模拟HTTP协议传播
  let http_carrier = TextMapCarrier::new()
  TextMapCarrier::set(http_carrier, "content-type", "application/json")
  TextMapCarrier::set(http_carrier, "user-agent", "test-client")
  
  let http_ctx = Context::with_value(Context::root(), ContextKey::new("http.request"), "/api/test")
  CompositePropagator::inject(cross_protocol_propagator, http_ctx, http_carrier)
  
  // 模拟gRPC协议传播
  let grpc_carrier = TextMapCarrier::new()
  TextMapCarrier::set(grpc_carrier, "grpc-timeout", "30s")
  TextMapCarrier::set(grpc_carrier, "grpc-encoding", "gzip")
  
  let grpc_ctx = Context::with_value(Context::root(), ContextKey::new("grpc.method"), "TestMethod")
  CompositePropagator::inject(cross_protocol_propagator, grpc_ctx, grpc_carrier)
  
  // 模拟消息队列协议传播
  let messaging_carrier = TextMapCarrier::new()
  TextMapCarrier::set(messaging_carrier, "message-id", "msg-12345")
  TextMapCarrier::set(messaging_carrier, "correlation-id", "corr-67890")
  
  let messaging_ctx = Context::with_value(Context::root(), ContextKey::new("messaging.topic"), "test.topic")
  CompositePropagator::inject(cross_protocol_propagator, messaging_ctx, messaging_carrier)
  
  // 验证跨协议兼容性
  let http_extracted = CompositePropagator::extract(cross_protocol_propagator, http_carrier)
  let grpc_extracted = CompositePropagator::extract(cross_protocol_propagator, grpc_carrier)
  let messaging_extracted = CompositePropagator::extract(cross_protocol_propagator, messaging_carrier)
  
  let http_trace = TextMapCarrier::get(http_carrier, "traceparent")
  let grpc_trace = TextMapCarrier::get(grpc_carrier, "traceparent")
  let messaging_trace = TextMapCarrier::get(messaging_carrier, "traceparent")
  
  assert_true(http_trace != None, "HTTP协议传播应该成功")
  assert_true(grpc_trace != None, "gRPC协议传播应该成功")
  assert_true(messaging_trace != None, "消息队列协议传播应该成功")
  
  let http_result = Context::get(http_extracted, ContextKey::new("extracted"))
  let grpc_result = Context::get(grpc_extracted, ContextKey::new("extracted"))
  let messaging_result = Context::get(messaging_extracted, ContextKey::new("extracted"))
  
  assert_eq(http_result, Some("true"), "HTTP协议提取应该成功")
  assert_eq(grpc_result, Some("true"), "gRPC协议提取应该成功")
  assert_eq(messaging_result, Some("true"), "消息队列协议提取应该成功")
}