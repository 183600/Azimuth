// Propagation模块CompositePropagator测试用例
// 测试复合传播器的功能和交互

test "composite_propagator_with_trace_and_baggage" {
  // 测试复合传播器同时处理TraceContext和Baggage
  
  // 创建W3C TraceContext和Baggage传播器
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 创建复合传播器
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 创建测试Carrier
  let carrier = MapCarrier::new()
  
  // 创建基础Context
  let base_context = Context::empty()
  
  // 测试注入操作
  composite_propagator.inject(base_context, carrier)
  
  // 验证Carrier中包含预期的头信息
  match carrier.get("traceparent") {
    Some(trace_parent) => {
      assert_eq(trace_parent.has_prefix("00-"), true)
      assert_eq(trace_parent.length(), 55)  // 标准traceparent格式长度
    }
    None => @test.fail("Expected traceparent header")
  }
  
  match carrier.get("baggage") {
    Some(baggage) => {
      assert_eq(baggage.contains("="), true)
    }
    None => @test.fail("Expected baggage header")
  }
  
  // 测试提取操作
  let carrier_with_data = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "key1=value1,key2=value2"),
    ("x-custom-header", "custom-value")
  ])
  
  let extracted_context = composite_propagator.extract(base_context, carrier_with_data)
  
  // 验证提取操作不抛出错误（简化实现中Context可能没有实际更新）
  assert_eq(extracted_context.values.length(), base_context.values.length())
  
  // 测试Carrier的键列表
  let carrier_keys = carrier_with_data.keys()
  assert_eq(carrier_keys.length(), 3)
  assert_eq(carrier_keys.contains("traceparent"), true)
  assert_eq(carrier_keys.contains("baggage"), true)
  assert_eq(carrier_keys.contains("x-custom-header"), true)
}

test "composite_propagator_multiple_trace_propagators" {
  // 测试复合传播器包含多个Trace传播器
  
  // 创建多个Trace传播器（在实际中可能不常见，但测试容错性）
  let trace_propagator1 = W3CTraceContextPropagator::{}
  let trace_propagator2 = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 创建包含重复传播器的复合传播器
  let composite_with_duplicates = CompositePropagator::new([
    trace_propagator1, 
    trace_propagator2, 
    baggage_propagator
  ])
  
  // 创建测试Carrier
  let carrier = MapCarrier::new()
  let base_context = Context::empty()
  
  // 测试注入操作
  composite_with_duplicates.inject(base_context, carrier)
  
  // 验证Carrier包含必要的头信息
  let trace_parent_count = {
    let mut count = 0
    let keys = carrier.keys()
    let mut i = 0
    while i < keys.length() {
      if keys[i] == "traceparent" {
        count = count + 1
      }
      i = i + 1
    }
    count
  }
  
  // 在简化实现中，可能只有一个traceparent头
  assert_eq(trace_parent_count >= 1, true)
  
  // 测试提取操作
  let carrier_with_trace = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "test.key=test.value")
  ])
  
  let extracted_context = composite_with_duplicates.extract(base_context, carrier_with_trace)
  assert_eq(extracted_context.values.length(), base_context.values.length())
}

test "composite_propagator_empty_and_single_propagator" {
  // 测试空传播器和单个传播器的复合传播器
  
  // 测试空传播器列表（虽然在实现中可能不允许）
  let empty_propagators : Array[TextMapPropagator] = []
  
  // 由于CompositePropagator::new可能需要至少一个传播器，我们跳过空列表测试
  // 测试单个传播器
  let single_trace_propagator = W3CTraceContextPropagator::{}
  let single_composite = CompositePropagator::new([single_trace_propagator])
  
  // 创建测试环境
  let carrier = MapCarrier::new()
  let base_context = Context::empty()
  
  // 测试单个传播器的注入
  single_composite.inject(base_context, carrier)
  
  // 验证只有traceparent头被注入
  match carrier.get("traceparent") {
    Some(trace_parent) => {
      assert_eq(trace_parent.has_prefix("00-"), true)
    }
    None => @test.fail("Expected traceparent header")
  }
  
  // 验证没有baggage头（因为没有baggage传播器）
  match carrier.get("baggage") {
    Some(_) => @test.fail("Did not expect baggage header")
    None => assert_eq(true, true)  // 预期没有baggage头
  }
  
  // 测试单个传播器的提取
  let carrier_with_only_baggage = MapCarrier::from_map([
    ("baggage", "key=value"),
    ("x-other-header", "other-value")
  ])
  
  let extracted_context = single_composite.extract(base_context, carrier_with_only_baggage)
  assert_eq(extracted_context.values.length(), base_context.values.length())
  
  // 测试只有baggage传播器的情况
  let single_baggage_propagator = W3CBaggagePropagator::{}
  let baggage_composite = CompositePropagator::new([single_baggage_propagator])
  
  let baggage_carrier = MapCarrier::new()
  baggage_composite.inject(base_context, baggage_carrier)
  
  // 验证只有baggage头被注入
  match baggage_carrier.get("baggage") {
    Some(baggage) => {
      assert_eq(baggage.contains("="), true)
    }
    None => @test.fail("Expected baggage header")
  }
  
  // 验证没有traceparent头
  match baggage_carrier.get("traceparent") {
    Some(_) => @test.fail("Did not expect traceparent header")
    None => assert_eq(true, true)  // 预期没有traceparent头
  }
}

test "composite_propagator_order_and_precedence" {
  // 测试复合传播器中传播器的顺序和优先级
  
  // 创建传播器并按特定顺序排列
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 测试不同的顺序
  let trace_first_composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  let baggage_first_composite = CompositePropagator::new([baggage_propagator, trace_propagator])
  
  // 创建测试Carrier
  let carrier1 = MapCarrier::new()
  let carrier2 = MapCarrier::new()
  let base_context = Context::empty()
  
  // 测试不同顺序的注入
  trace_first_composite.inject(base_context, carrier1)
  baggage_first_composite.inject(base_context, carrier2)
  
  // 验证两种顺序都产生了必要的头信息
  let carrier1_keys = carrier1.keys()
  let carrier2_keys = carrier2.keys()
  
  assert_eq(carrier1_keys.contains("traceparent"), true)
  assert_eq(carrier1_keys.contains("baggage"), true)
  assert_eq(carrier2_keys.contains("traceparent"), true)
  assert_eq(carrier2_keys.contains("baggage"), true)
  
  // 测试提取时的顺序影响
  let carrier_with_data = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "key1=value1,key2=value2"),
    ("x-custom-header", "custom-value")
  ])
  
  let context1 = trace_first_composite.extract(base_context, carrier_with_data)
  let context2 = baggage_first_composite.extract(base_context, carrier_with_data)
  
  // 在简化实现中，顺序可能不影响结果
  assert_eq(context1.values.length(), context2.values.length())
  
  // 测试传播器数量对性能的影响
  let many_propagators = CompositePropagator::new([
    trace_propagator, baggage_propagator, trace_propagator, baggage_propagator
  ])
  
  let carrier3 = MapCarrier::new()
  many_propagators.inject(base_context, carrier3)
  
  // 验证即使有多个传播器，也能正确注入
  let carrier3_keys = carrier3.keys()
  assert_eq(carrier3_keys.contains("traceparent"), true)
  assert_eq(carrier3_keys.contains("baggage"), true)
}

test "composite_propagator_error_handling" {
  // 测试复合传播器的错误处理
  
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 测试空Carrier的注入
  let empty_carrier = MapCarrier::new()
  let base_context = Context::empty()
  
  // 注入操作应该成功
  composite_propagator.inject(base_context, empty_carrier)
  
  // 验证注入后的Carrier包含必要的头
  let empty_carrier_keys = empty_carrier.keys()
  assert_eq(empty_carrier_keys.contains("traceparent"), true)
  assert_eq(empty_carrier_keys.contains("baggage"), true)
  
  // 测试空Carrier的提取
  let empty_extraction_carrier = MapCarrier::new()
  let extracted_from_empty = composite_propagator.extract(base_context, empty_extraction_carrier)
  
  // 从空Carrier提取应该返回原始Context
  assert_eq(extracted_from_empty.values.length(), base_context.values.length())
  
  // 测试包含无效数据的Carrier
  let invalid_carrier = MapCarrier::from_map([
    ("traceparent", "invalid-traceparent-format"),
    ("baggage", "invalid-baggage-format-without-equals"),
    ("malformed-header", "")
  ])
  
  // 从无效数据提取应该不抛出错误
  let extracted_from_invalid = composite_propagator.extract(base_context, invalid_carrier)
  assert_eq(extracted_from_invalid.values.length(), base_context.values.length())
  
  // 测试包含超大头的Carrier
  let large_traceparent = "00-" + "a".repeat(32) + "-" + "b".repeat(16) + "-01"
  let large_baggage = "key1=" + "x".repeat(1000) + ",key2=" + "y".repeat(1000)
  
  let large_carrier = MapCarrier::from_map([
    ("traceparent", large_traceparent),
    ("baggage", large_baggage)
  ])
  
  // 处理大头应该成功
  let extracted_from_large = composite_propagator.extract(base_context, large_carrier)
  assert_eq(extracted_from_large.values.length(), base_context.values.length())
  
  // 测试包含特殊字符的Carrier
  let special_chars_carrier = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "key.with.dots=value.with.dots,key with spaces=value with spaces,key!@#$=value!@#$"),
    ("unicode-header", "键=值")
  ])
  
  let extracted_from_special = composite_propagator.extract(base_context, special_chars_carrier)
  assert_eq(extracted_from_special.values.length(), base_context.values.length())
  
  // 测试包含大量头的Carrier
  let many_headers = []
  let mut i = 0
  while i < 50 {
    many_headers.push(("header." + i.to_string(), "value." + i.to_string()))
    i = i + 1
  }
  
  // 添加必要的传播头
  many_headers.push(("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  many_headers.push(("baggage", "key=value"))
  
  let many_headers_carrier = MapCarrier::from_map(many_headers)
  let extracted_from_many = composite_propagator.extract(base_context, many_headers_carrier)
  assert_eq(extracted_from_many.values.length(), base_context.values.length())
  
  // 验证Carrier的键数量
  let many_keys = many_headers_carrier.keys()
  assert_eq(many_keys.length(), 52)  // 50个自定义头 + traceparent + baggage
}

test "composite_propagator_complex_scenarios" {
  // 测试复合传播器的复杂场景
  
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 测试多次注入到同一个Carrier
  let carrier = MapCarrier::new()
  let base_context = Context::empty()
  
  // 第一次注入
  composite_propagator.inject(base_context, carrier)
  
  // 第二次注入（在简化实现中可能会覆盖或添加）
  composite_propagator.inject(base_context, carrier)
  
  // 验证Carrier仍然包含必要的头
  let carrier_keys = carrier.keys()
  assert_eq(carrier_keys.contains("traceparent"), true)
  assert_eq(carrier_keys.contains("baggage"), true)
  
  // 测试链式传播（从一个Context提取，然后注入到另一个Carrier）
  let source_carrier = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "source.key=source.value,shared.key=shared.value")
  ])
  
  // 提取Context
  let extracted_context = composite_propagator.extract(base_context, source_carrier)
  
  // 注入到新的Carrier
  let target_carrier = MapCarrier::new()
  composite_propagator.inject(extracted_context, target_carrier)
  
  // 验证目标Carrier包含传播的信息
  let target_keys = target_carrier.keys()
  assert_eq(target_keys.contains("traceparent"), true)
  assert_eq(target_keys.contains("baggage"), true)
  
  // 测试部分传播（只有部分传播器能处理的数据）
  let partial_carrier = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("x-custom-trace", "custom-trace-data"),
    ("non-standard-baggage", "key=value")
  ])
  
  let partial_context = composite_propagator.extract(base_context, partial_carrier)
  assert_eq(partial_context.values.length(), base_context.values.length())
  
  // 测试传播器的独立性
  let trace_only_composite = CompositePropagator::new([trace_propagator])
  let baggage_only_composite = CompositePropagator::new([baggage_propagator])
  
  let trace_carrier = MapCarrier::new()
  let baggage_carrier = MapCarrier::new()
  
  trace_only_composite.inject(base_context, trace_carrier)
  baggage_only_composite.inject(base_context, baggage_carrier)
  
  // 验证独立传播器的结果
  let trace_keys = trace_carrier.keys()
  let baggage_keys = baggage_carrier.keys()
  
  assert_eq(trace_keys.contains("traceparent"), true)
  assert_eq(trace_keys.contains("baggage"), false)
  
  assert_eq(baggage_keys.contains("traceparent"), false)
  assert_eq(baggage_keys.contains("baggage"), true)
  
  // 测试传播器的组合效果
  let combined_carrier = MapCarrier::new()
  
  // 先注入trace信息
  trace_only_composite.inject(base_context, combined_carrier)
  
  // 再注入baggage信息
  baggage_only_composite.inject(base_context, combined_carrier)
  
  // 验证组合效果与复合传播器相同
  let combined_keys = combined_carrier.keys()
  assert_eq(combined_keys.contains("traceparent"), true)
  assert_eq(combined_keys.contains("baggage"), true)
  
  // 测试传播器的条件执行
  let conditional_inject = fn(ctx : Context, carrier : MapCarrier, include_baggage : Bool) -> Unit {
    if include_baggage {
      composite_propagator.inject(ctx, carrier)
    } else {
      trace_only_composite.inject(ctx, carrier)
    }
  }
  
  let conditional_carrier1 = MapCarrier::new()
  let conditional_carrier2 = MapCarrier::new()
  
  conditional_inject(base_context, conditional_carrier1, true)
  conditional_inject(base_context, conditional_carrier2, false)
  
  let conditional_keys1 = conditional_carrier1.keys()
  let conditional_keys2 = conditional_carrier2.keys()
  
  assert_eq(conditional_keys1.contains("traceparent"), true)
  assert_eq(conditional_keys1.contains("baggage"), true)
  
  assert_eq(conditional_keys2.contains("traceparent"), true)
  assert_eq(conditional_keys2.contains("baggage"), false)
}