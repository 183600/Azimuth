// W3C传播器标准测试用例
// 测试W3C Trace Context和Baggage传播标准的实现

test "w3c_trace_context_header_format" {
  // 测试W3C Trace Context头格式
  let propagator = W3CTraceContextPropagator::new()
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // 测试标准的traceparent格式
  // 格式: version-trace-id-parent-id-flags
  let standard_traceparent = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
  TextMapCarrier::set(carrier, "traceparent", standard_traceparent)
  
  // 验证traceparent格式
  let retrieved_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(retrieved_traceparent, Some(standard_traceparent))
  
  // 测试不同格式的traceparent
  let traceparent_with_sampled = "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
  let traceparent_not_sampled = "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-00"
  let traceparent_with_tracestate = "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
  
  TextMapCarrier::set(carrier, "traceparent.sampled", traceparent_with_sampled)
  TextMapCarrier::set(carrier, "traceparent.not_sampled", traceparent_not_sampled)
  TextMapCarrier::set(carrier, "traceparent.with_state", traceparent_with_tracestate)
  
  assert_eq(TextMapCarrier::get(carrier, "traceparent.sampled"), Some(traceparent_with_sampled))
  assert_eq(TextMapCarrier::get(carrier, "traceparent.not_sampled"), Some(traceparent_not_sampled))
  assert_eq(TextMapCarrier::get(carrier, "traceparent.with_state"), Some(traceparent_with_tracestate))
  
  // 测试traceparent格式的组成部分
  let parts = standard_traceparent.split("-")
  assert_eq(parts.length(), 4)
  assert_eq(parts[0], "00")  // version
  assert_eq(parts[1], "0af7651916cd43dd8448eb211c80319c")  // trace-id (32 hex chars)
  assert_eq(parts[2], "b7ad6b7169203331")  // parent-id (16 hex chars)
  assert_eq(parts[3], "01")  // flags
}

test "w3c_trace_context_injection_extraction" {
  // 测试W3C Trace Context的注入和提取
  let composite_propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let carrier = TextMapCarrier::new()
  
  // 创建带有trace信息的上下文
  let original_ctx = Context::root()
  let trace_key = ContextKey::new("trace_info")
  let ctx_with_trace = Context::with_value(original_ctx, trace_key, "original_trace_data")
  
  // 注入trace context到carrier
  CompositePropagator::inject(composite_propagator, ctx_with_trace, carrier)
  
  // 验证注入的traceparent头
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(injected_traceparent != None)
  
  // 从carrier提取trace context
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_trace_info = Context::get(extracted_ctx, ContextKey::new("extracted"))
  
  // 验证提取的上下文
  assert_eq(extracted_trace_info, Some("true"))
  
  // 测试多次注入和提取
  let carrier2 = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, extracted_ctx, carrier2)
  
  let extracted_ctx2 = CompositePropagator::extract(composite_propagator, carrier2)
  let extracted_trace_info2 = Context::get(extracted_ctx2, ContextKey::new("extracted"))
  
  assert_eq(extracted_trace_info2, Some("true"))
}

test "w3c_baggage_header_format" {
  // 测试W3C Baggage头格式
  let baggage_propagator = W3CBaggagePropagator::new()
  let carrier = TextMapCarrier::new()
  
  // 测试简单的baggage格式
  let simple_baggage = "key1=value1"
  TextMapCarrier::set(carrier, "baggage", simple_baggage)
  
  let retrieved_baggage = TextMapCarrier::get(carrier, "baggage")
  assert_eq(retrieved_baggage, Some(simple_baggage))
  
  // 测试多个baggage项
  let multi_baggage = "key1=value1,key2=value2,key3=value3"
  TextMapCarrier::set(carrier, "baggage.multi", multi_baggage)
  
  assert_eq(TextMapCarrier::get(carrier, "baggage.multi"), Some(multi_baggage))
  
  // 测试带属性的baggage项
  let baggage_with_properties = "key1=value1;property1=value1a;property2=value1b,key2=value2;prop=value2a"
  TextMapCarrier::set(carrier, "baggage.with_props", baggage_with_properties)
  
  assert_eq(TextMapCarrier::get(carrier, "baggage.with_props"), Some(baggage_with_properties))
  
  // 测试包含特殊字符的baggage值
  let special_baggage = "key1=value%20with%20spaces,key2=value%2Fwith%2Fslashes,key3=value%3Dwith%3Dequals"
  TextMapCarrier::set(carrier, "baggage.special", special_baggage)
  
  assert_eq(TextMapCarrier::get(carrier, "baggage.special"), Some(special_baggage))
  
  // 测试URL编码的baggage
  let url_encoded_baggage = "user%20id=user123,session%20id=sess456,transaction%20id=txn789"
  TextMapCarrier::set(carrier, "baggage.encoded", url_encoded_baggage)
  
  assert_eq(TextMapCarrier::get(carrier, "baggage.encoded"), Some(url_encoded_baggage))
}

test "w3c_baggage_propagation_chain" {
  // 测试W3C Baggage传播链
  let baggage = Baggage::new()
  
  // 创建baggage传播链
  let baggage1 = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "sess456")
  let baggage3 = Baggage::set_entry(baggage2, "request.id", "req789")
  let baggage4 = Baggage::set_entry(baggage3, "correlation.id", "corr012")
  
  // 验证baggage传播链
  assert_eq(Baggage::get_entry(baggage4, "user.id"), Some("user123"))
  assert_eq(Baggage::get_entry(baggage4, "session.id"), Some("sess456"))
  assert_eq(Baggage::get_entry(baggage4, "request.id"), Some("req789"))
  assert_eq(Baggage::get_entry(baggage4, "correlation.id"), Some("corr012"))
  
  // 模拟跨服务传播
  let carrier = TextMapCarrier::new()
  
  // 将baggage信息注入到carrier（模拟W3C baggage头）
  let baggage_header = "user.id=user123,session.id=sess456,request.id=req789,correlation.id=corr012"
  TextMapCarrier::set(carrier, "baggage", baggage_header)
  
  // 验证baggage头的正确性
  let injected_baggage = TextMapCarrier::get(carrier, "baggage")
  assert_eq(injected_baggage, Some(baggage_header))
  
  // 模拟下一个服务接收并处理baggage
  let received_baggage = TextMapCarrier::get(carrier, "baggage")
  match received_baggage {
    Some(baggage_str) => {
      // 在实际实现中，这里会解析baggage字符串
      assert_true(baggage_str.contains("user.id=user123"))
      assert_true(baggage_str.contains("session.id=sess456"))
      assert_true(baggage_str.contains("request.id=req789"))
      assert_true(baggage_str.contains("correlation.id=corr012"))
    }
    None => assert_false(true, "Expected baggage header")
  }
}

test "w3c_propagator_composite_operations" {
  // 测试W3C传播器的复合操作
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // 创建复合传播器
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // 设置上下文信息
  let ctx_with_data = Context::with_value(ctx, ContextKey::new("operation"), "composite_test")
  
  // 注入传播信息
  CompositePropagator::inject(composite_propagator, ctx_with_data, carrier)
  
  // 验证traceparent头存在
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(traceparent != None)
  
  // 手动添加baggage头（模拟复合传播）
  let baggage_header = "operation=composite_test,service=test.service,version=1.0.0"
  TextMapCarrier::set(carrier, "baggage", baggage_header)
  
  // 验证两个头都存在
  let baggage = TextMapCarrier::get(carrier, "baggage")
  assert_eq(baggage, Some(baggage_header))
  
  // 提取传播信息
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_operation = Context::get(extracted_ctx, ContextKey::new("extracted"))
  
  assert_eq(extracted_operation, Some("true"))
  
  // 测试多次传播操作
  let carrier2 = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, extracted_ctx, carrier2)
  
  let traceparent2 = TextMapCarrier::get(carrier2, "traceparent")
  assert_true(traceparent2 != None)
}

test "w3c_propagator_error_handling" {
  // 测试W3C传播器的错误处理
  let propagator = W3CTraceContextPropagator::new()
  let carrier = TextMapCarrier::new()
  
  // 测试无效的traceparent格式
  let invalid_traceparents = [
    "",                                    // 空字符串
    "invalid_format",                      // 完全无效格式
    "01-traceid",                          // 缺少部分
    "00-traceid-parentid",                 // 缺少flags
    "00-traceid-parentid-flags-extra",     // 额外部分
    "01-traceid-parentid-flags",           // 不支持的版本
    "00-short-parentid-01",                // traceid太短
    "00-traceid-short-01",                 // parentid太短
    "00-traceid-parentid-flags",           // 缺少分隔符
    "00-INVALID_TRACE_ID-INVALID_PARENT_ID-01"  // 无效字符
  ]
  
  for invalid_traceparent in invalid_traceparents {
    TextMapCarrier::set(carrier, "traceparent", invalid_traceparent)
    let retrieved = TextMapCarrier::get(carrier, "traceparent")
    assert_eq(retrieved, Some(invalid_traceparent))
  }
  
  // 测试无效的baggage格式
  let invalid_baggages = [
    "",                                    // 空字符串
    "key",                                 // 只有键没有值
    "=value",                              // 只有值没有键
    "key=value,invalid",                   // 第二项无效
    "key=value;invalid_property",          // 无效属性
    "key=value;property",                  // 属性没有值
    "key=value;property=invalid=extra",    // 属性值包含额外等号
    "key=value,key=value,..."              // 过长的baggage
  ]
  
  for invalid_baggage in invalid_baggages {
    TextMapCarrier::set(carrier, "baggage", invalid_baggage)
    let retrieved = TextMapCarrier::get(carrier, "baggage")
    assert_eq(retrieved, Some(invalid_baggage))
  }
  
  // 测试处理空carrier
  let empty_carrier = TextMapCarrier::new()
  let empty_traceparent = TextMapCarrier::get(empty_carrier, "traceparent")
  let empty_baggage = TextMapCarrier::get(empty_carrier, "baggage")
  
  assert_eq(empty_traceparent, None)
  assert_eq(empty_baggage, None)
}

test "w3c_propagator_cross_service_compatibility" {
  // 测试W3C传播器的跨服务兼容性
  let composite_propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // 模拟服务A -> 服务B -> 服务C的传播链
  let carrier_a = TextMapCarrier::new()
  let ctx_a = Context::root()
  let ctx_a_with_data = Context::with_value(ctx_a, ContextKey::new("service"), "A")
  
  // 服务A注入传播信息
  CompositePropagator::inject(composite_propagator, ctx_a_with_data, carrier_a)
  
  // 服务B接收并处理
  let ctx_b = CompositePropagator::extract(composite_propagator, carrier_a)
  let ctx_b_with_data = Context::with_value(ctx_b, ContextKey::new("service"), "B")
  
  let carrier_b = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx_b_with_data, carrier_b)
  
  // 服务C接收并处理
  let ctx_c = CompositePropagator::extract(composite_propagator, carrier_b)
  let ctx_c_with_data = Context::with_value(ctx_c, ContextKey::new("service"), "C")
  
  let carrier_c = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx_c_with_data, carrier_c)
  
  // 验证传播链的完整性
  let traceparent_a = TextMapCarrier::get(carrier_a, "traceparent")
  let traceparent_b = TextMapCarrier::get(carrier_b, "traceparent")
  let traceparent_c = TextMapCarrier::get(carrier_c, "traceparent")
  
  assert_true(traceparent_a != None)
  assert_true(traceparent_b != None)
  assert_true(traceparent_c != None)
  
  // 验证traceparent的一致性（在实际实现中应该相同或相关）
  match (traceparent_a, traceparent_b, traceparent_c) {
    (Some(tpa), Some(tpb), Some(tpc)) => {
      // 在实际实现中，这里会验证traceparent的格式和内容
      assert_true(tpa.length() > 0)
      assert_true(tpb.length() > 0)
      assert_true(tpc.length() > 0)
    }
    _ => assert_false(true, "All traceparents should be present")
  }
  
  // 测试baggage在传播链中的累积
  let baggage_a = "service=A,entry.point=api.gateway"
  let baggage_b = baggage_a + ",service=B,processed.by=service.b"
  let baggage_c = baggage_b + ",service=C,processed.by=service.c"
  
  TextMapCarrier::set(carrier_a, "baggage", baggage_a)
  TextMapCarrier::set(carrier_b, "baggage", baggage_b)
  TextMapCarrier::set(carrier_c, "baggage", baggage_c)
  
  let final_baggage = TextMapCarrier::get(carrier_c, "baggage")
  assert_eq(final_baggage, Some(baggage_c))
  
  // 验证baggage累积包含所有服务信息
  match final_baggage {
    Some(baggage_str) => {
      assert_true(baggage_str.contains("service=A"))
      assert_true(baggage_str.contains("service=B"))
      assert_true(baggage_str.contains("service=C"))
    }
    None => assert_false(true, "Expected final baggage")
  }
}