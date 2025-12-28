// 高级复合传播器测试用例
// 测试复合传播器在复杂场景下的行为和功能

pub test "multiple propagators injection and extraction" {
  // 创建多个传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  // 创建包含多个传播器的复合传播器
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 准备上下文
  let ctx = azimuth::Context::root()
  
  // 注入到carrier
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // 验证注入结果
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  assert_true(trace_header != None, "traceparent头部应该存在")
  
  // 提取上下文
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // 验证提取结果
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

pub test "propagator order and precedence" {
  // 创建传播器并测试顺序
  let trace_propagator1 = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator1 = azimuth::W3CBaggagePropagator::new()
  
  // 测试不同顺序的传播器组合
  let propagators_order1 = [trace_propagator1, baggage_propagator1]
  let composite_propagator1 = azimuth::CompositePropagator::new(propagators_order1)
  
  let trace_propagator2 = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator2 = azimuth::W3CBaggagePropagator::new()
  
  let propagators_order2 = [baggage_propagator2, trace_propagator2]
  let composite_propagator2 = azimuth::CompositePropagator::new(propagators_order2)
  
  // 测试注入
  let ctx = azimuth::Context::root()
  let carrier1 = azimuth::TextMapCarrier::new()
  let carrier2 = azimuth::TextMapCarrier::new()
  
  azimuth::CompositePropagator::inject(composite_propagator1, ctx, carrier1)
  azimuth::CompositePropagator::inject(composite_propagator2, ctx, carrier2)
  
  // 测试提取
  let extracted_ctx1 = azimuth::CompositePropagator::extract(composite_propagator1, carrier1)
  let extracted_ctx2 = azimuth::CompositePropagator::extract(composite_propagator2, carrier2)
  
  // 验证提取结果
  let extracted_key = azimuth::ContextKey::new("extracted")
  let value1 = azimuth::Context::get(extracted_ctx1, extracted_key)
  let value2 = azimuth::Context::get(extracted_ctx2, extracted_key)
  
  assert_eq(value1, Some("true"))
  assert_eq(value2, Some("true"))
}

pub test "empty propagator list handling" {
  // 测试空传播器列表
  let empty_propagators = []
  let empty_composite_propagator = azimuth::CompositePropagator::new(empty_propagators)
  
  let ctx = azimuth::Context::root()
  let carrier = azimuth::TextMapCarrier::new()
  
  // 测试注入（应该不执行任何操作）
  azimuth::CompositePropagator::inject(empty_composite_propagator, ctx, carrier)
  
  // 测试提取（应该返回原始上下文）
  let extracted_ctx = azimuth::CompositePropagator::extract(empty_composite_propagator, carrier)
  
  // 验证结果
  let test_key = azimuth::ContextKey::new("test")
  let original_value = azimuth::Context::get(ctx, test_key)
  let extracted_value = azimuth::Context::get(extracted_ctx, test_key)
  
  assert_eq(original_value, extracted_value)
}

pub test "single propagator in composite" {
  // 测试单个传播器的复合传播器
  let single_propagator = azimuth::W3CTraceContextPropagator::new()
  let single_propagators = [single_propagator]
  let single_composite_propagator = azimuth::CompositePropagator::new(single_propagators)
  
  let ctx = azimuth::Context::root()
  let carrier = azimuth::TextMapCarrier::new()
  
  // 测试注入
  azimuth::CompositePropagator::inject(single_composite_propagator, ctx, carrier)
  
  // 验证注入结果
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  assert_true(trace_header != None, "traceparent头部应该存在")
  
  // 测试提取
  let extracted_ctx = azimuth::CompositePropagator::extract(single_composite_propagator, carrier)
  
  // 验证提取结果
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

pub test "propagator with context data" {
  // 创建带有数据的上下文
  let ctx = azimuth::Context::root()
  let user_key = azimuth::ContextKey::new("user.id")
  let request_key = azimuth::ContextKey::new("request.id")
  
  let ctx_with_data = azimuth::Context::with_value(ctx, user_key, "user-12345")
  let ctx_with_more_data = azimuth::Context::with_value(ctx_with_data, request_key, "req-67890")
  
  // 创建传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 测试注入
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, ctx_with_more_data, carrier)
  
  // 测试提取
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // 验证原始上下文数据仍然存在
  let original_user_value = azimuth::Context::get(ctx_with_more_data, user_key)
  let original_request_value = azimuth::Context::get(ctx_with_more_data, request_key)
  
  assert_eq(original_user_value, Some("user-12345"))
  assert_eq(original_request_value, Some("req-67890"))
  
  // 验证提取后的上下文包含传播的数据
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

pub test "propagator error handling" {
  // 测试传播器错误处理
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  let ctx = azimuth::Context::root()
  
  // 测试无效carrier
  let invalid_carrier = azimuth::TextMapCarrier::new()
  
  // 注入应该正常工作
  azimuth::CompositePropagator::inject(composite_propagator, ctx, invalid_carrier)
  
  // 提取应该正常工作
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, invalid_carrier)
  
  // 验证结果
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

pub test "propagator with multiple injection extraction cycles" {
  // 测试多次注入提取循环
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  let mut current_ctx = azimuth::Context::root()
  
  // 执行多次注入提取循环
  for i in 0..5 {
    let carrier = azimuth::TextMapCarrier::new()
    
    // 注入
    azimuth::CompositePropagator::inject(composite_propagator, current_ctx, carrier)
    
    // 提取
    current_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
    
    // 验证每次循环都成功
    let extracted_key = azimuth::ContextKey::new("extracted")
    let extracted_value = azimuth::Context::get(current_ctx, extracted_key)
    assert_eq(extracted_value, Some("true"))
  }
}

pub test "propagator with baggage operations" {
  // 测试baggage操作与传播器的集成
  let baggage = azimuth::Baggage::new()
  let baggage_with_entries = azimuth::Baggage::set_entry(baggage, "user.id", "user-123")
  let final_baggage = azimuth::Baggage::set_entry(baggage_with_entries, "request.id", "req-456")
  
  // 创建传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  let ctx = azimuth::Context::root()
  
  // 测试注入
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // 验证baggage条目
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "user.id"), Some("user-123"))
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "request.id"), Some("req-456"))
  
  // 测试提取
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // 验证提取结果
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

pub test "propagator with span context integration" {
  // 测试传播器与span上下文的集成
  let span_ctx = azimuth::SpanContext::new("trace-123456", "span-789012", true, "sampled=1")
  
  // 验证span上下文
  assert_true(azimuth::SpanContext::is_valid(span_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_ctx))
  assert_eq(azimuth::SpanContext::trace_id(span_ctx), "trace-123456")
  assert_eq(azimuth::SpanContext::span_id(span_ctx), "span-789012")
  assert_eq(azimuth::SpanContext::trace_state(span_ctx), "sampled=1")
  
  // 创建传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  let ctx = azimuth::Context::root()
  
  // 测试注入
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // 验证trace头部
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  assert_true(trace_header != None, "traceparent头部应该存在")
  
  // 测试提取
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // 验证提取结果
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

pub test "propagator performance with multiple operations" {
  // 测试传播器性能
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  let ctx = azimuth::Context::root()
  
  // 测试多次注入操作的性能
  let start_inject_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i in 0..100 {
    let carrier = azimuth::TextMapCarrier::new()
    azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
  }
  
  let end_inject_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let inject_duration = end_inject_time - start_inject_time
  
  // 验证注入操作在合理时间内完成
  assert_true(inject_duration < 5000000000L, "100次注入操作应该在5秒内完成")
  
  // 测试多次提取操作的性能
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  let start_extract_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i in 0..100 {
    let _ = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  }
  
  let end_extract_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let extract_duration = end_extract_time - start_extract_time
  
  // 验证提取操作在合理时间内完成
  assert_true(extract_duration < 5000000000L, "100次提取操作应该在5秒内完成")
}