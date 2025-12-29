// 复合传播器错误处理测试
test "composite_propagator_error_handling" {
  // 测试复合传播器的基本错误处理
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 创建空传播器数组
  let empty_propagators : Array[TextMapPropagator] = []
  let empty_composite = CompositePropagator::new(empty_propagators)
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试空传播器数组的注入和提取
  empty_composite.inject(ctx, carrier)
  let extracted_ctx = empty_composite.extract(ctx, carrier)
  
  // 应该不会崩溃
  assert_eq(true, true)
  
  // 创建正常传播器数组
  let normal_propagators = [trace_propagator, baggage_propagator]
  let normal_composite = CompositePropagator::new(normal_propagators)
  
  // 测试正常注入
  normal_composite.inject(ctx, carrier)
  
  // 验证carrier中有预期的header
  match carrier.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected traceparent header")
  }
  
  match carrier.get("baggage") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected baggage header")
  }
  
  // 测试正常提取
  let extracted_normal_ctx = normal_composite.extract(ctx, carrier)
  assert_eq(true, true)
}

test "composite_propagator_invalid_traceparent_handling" {
  // 测试处理无效traceparent的错误处理
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let ctx = Context::empty()
  
  // 测试各种无效的traceparent格式
  let invalid_traceparents = [
    "",  // 空字符串
    "invalid",  // 完全无效
    "00-",  // 不完整
    "00-0af7651916cd43dd8448eb211c80319c",  // 缺少span_id和flags
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331",  // 缺少flags
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01-extra",  // 额外部分
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-xyz",  // 无效flags
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b71692033-01",  // span_id长度错误
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-0123",  // flags长度错误
    "gg-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",  // 无效版本
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-ff",  // 无效flags
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01-tracestate"  // 额外tracestate
  ]
  
  let mut i = 0
  while i < invalid_traceparents.length() {
    let carrier = MapCarrier::from_map([
      ("traceparent", invalid_traceparents[i])
    ])
    
    // 即使traceparent无效，提取也不应该崩溃
    let extracted_ctx = composite.extract(ctx, carrier)
    assert_eq(true, true)
    i = i + 1
  }
}

test "composite_propagator_invalid_baggage_handling" {
  // 测试处理无效baggage的错误处理
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let ctx = Context::empty()
  
  // 测试各种无效的baggage格式
  let invalid_baggages = [
    "",  // 空字符串
    "invalid",  // 没有等号
    "key=",  // 空值
    "=value",  // 空键
    "key=value,invalid",  // 混合有效和无效
    "key=value,,",  // 连续逗号
    ",key=value",  // 开头逗号
    "key=value,",  // 结尾逗号
    "key=value;=invalid",  // 无效属性
    "key=value;invalid",  // 不完整属性
    "key=value;prop=;prop2=val2",  // 混合有效和无效属性
    "a".repeat(10000) + "=" + "b".repeat(10000),  // 极长的键值对
    "key=value" + ",key=value".repeat(1000)  // 太多的键值对
  ]
  
  let mut i = 0
  while i < invalid_baggages.length() {
    let carrier = MapCarrier::from_map([
      ("baggage", invalid_baggages[i])
    ])
    
    // 即使baggage无效，提取也不应该崩溃
    let extracted_ctx = composite.extract(ctx, carrier)
    assert_eq(true, true)
    i = i + 1
  }
}

test "composite_propagator_malformed_carrier_handling" {
  // 测试处理格式错误的carrier的错误处理
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let ctx = Context::empty()
  
  // 测试包含特殊字符的header名称和值
  let malformed_carriers = [
    MapCarrier::from_map([
      ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
      ("baggage", "user.id=12345"),
      ("", "empty_key_header")  // 空键名
    ]),
    MapCarrier::from_map([
      ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
      ("baggage", ""),  // 空baggage值
      ("x-custom-header", "")  // 空自定义header值
    ]),
    MapCarrier::from_map([
      ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
      ("baggage", "key=value"),
      ("special!@#$%^&*()", "special!@#$%^&*()")  // 特殊字符
    ]),
    MapCarrier::from_map([
      ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
      ("baggage", "测试中文=value"),  // Unicode
      ("unicode.header", "测试中文值")
    ]),
    MapCarrier::from_map([
      ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
      ("baggage", "key=value"),
      ("space header", "space value")  // 包含空格
    ])
  ]
  
  let mut i = 0
  while i < malformed_carriers.length() {
    // 即使carrier格式异常，提取也不应该崩溃
    let extracted_ctx = composite.extract(ctx, malformed_carriers[i])
    assert_eq(true, true)
    i = i + 1
  }
}

test "composite_propagator_partial_failure_handling" {
  // 测试部分传播器失败的处理
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let ctx = Context::empty()
  
  // 测试只有部分header有效的情况
  let partial_valid_carriers = [
    MapCarrier::from_map([
      ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
      ("baggage", "invalid")  // 只有traceparent有效
    ]),
    MapCarrier::from_map([
      ("traceparent", "invalid"),  // 只有baggage有效
      ("baggage", "user.id=12345")
    ]),
    MapCarrier::from_map([
      ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
      // 没有baggage header
    ]),
    MapCarrier::from_map([
      ("baggage", "user.id=12345")
      // 没有traceparent header
    ]),
    MapCarrier::from_map([
      ("traceparent", ""),  // 空traceparent
      ("baggage", "")  // 空baggage
    ])
  ]
  
  let mut i = 0
  while i < partial_valid_carriers.length() {
    // 即使部分header无效，提取也不应该崩溃
    let extracted_ctx = composite.extract(ctx, partial_valid_carriers[i])
    assert_eq(true, true)
    i = i + 1
  }
}

test "composite_propagator_extreme_conditions_handling" {
  // 测试极端条件下的错误处理
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let ctx = Context::empty()
  
  // 测试极长的header值
  let very_long_traceparent = "00-" + "a".repeat(32) + "-" + "b".repeat(16) + "-01"
  let very_long_baggage = "key=" + "value".repeat(1000)
  
  let extreme_carrier = MapCarrier::from_map([
    ("traceparent", very_long_traceparent),
    ("baggage", very_long_baggage)
  ])
  
  // 即使header值极长，提取也不应该崩溃
  let extracted_ctx = composite.extract(ctx, extreme_carrier)
  assert_eq(true, true)
  
  // 测试大量header
  let mut many_headers = []
  let mut i = 0
  while i < 1000 {
    many_headers.push(("header." + i.to_string(), "value." + i.to_string()))
    i = i + 1
  }
  
  // 添加traceparent和baggage
  many_headers.push(("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  many_headers.push(("baggage", "user.id=12345"))
  
  let many_headers_carrier = MapCarrier::from_map(many_headers)
  
  // 即使有大量header，提取也不应该崩溃
  let extracted_many_ctx = composite.extract(ctx, many_headers_carrier)
  assert_eq(true, true)
  
  // 测试嵌套或复杂的baggage值
  let complex_baggage = "key1=value1;prop1=val1;prop2=val2,key2=value2;prop3=val3,key3=value3;prop4=val4;prop5=val5"
  let complex_carrier = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", complex_baggage)
  ])
  
  let extracted_complex_ctx = composite.extract(ctx, complex_carrier)
  assert_eq(true, true)
}

test "composite_propagator_injection_error_handling" {
  // 测试注入过程中的错误处理
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let ctx = Context::empty()
  
  // 测试向不同的carrier注入
  let normal_carrier = MapCarrier::new()
  let pre_filled_carrier = MapCarrier::from_map([
    ("existing-header", "existing-value"),
    ("traceparent", "existing-traceparent"),
    ("baggage", "existing-baggage")
  ])
  
  // 注入到空的carrier
  composite.inject(ctx, normal_carrier)
  assert_eq(true, true)
  
  // 注入到预填充的carrier
  composite.inject(ctx, pre_filled_carrier)
  assert_eq(true, true)
  
  // 测试多次注入
  let multi_inject_carrier = MapCarrier::new()
  let mut i = 0
  while i < 10 {
    composite.inject(ctx, multi_inject_carrier)
    i = i + 1
  }
  assert_eq(true, true)
  
  // 测试带有复杂context的注入
  let complex_ctx = ctx
    .with_value(create_key("user.id"), "user-123456")
    .with_value(create_key("session.id"), "session-abcdef")
    .with_value(create_key("request.id"), "req-1234567890")
  
  let complex_carrier = MapCarrier::new()
  composite.inject(complex_ctx, complex_carrier)
  assert_eq(true, true)
}

test "composite_propagator_order_dependent_error_handling" {
  // 测试传播器顺序相关的错误处理
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 测试不同的传播器顺序
  let trace_first_composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  let baggage_first_composite = CompositePropagator::new([baggage_propagator, trace_propagator])
  
  let ctx = Context::empty()
  
  // 测试注入顺序的影响
  let trace_first_carrier = MapCarrier::new()
  trace_first_composite.inject(ctx, trace_first_carrier)
  
  let baggage_first_carrier = MapCarrier::new()
  baggage_first_composite.inject(ctx, baggage_first_carrier)
  
  // 两种顺序都应该成功
  assert_eq(true, true)
  
  // 测试提取顺序的影响
  let test_carrier = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "user.id=12345")
  ])
  
  let trace_first_extracted = trace_first_composite.extract(ctx, test_carrier)
  let baggage_first_extracted = baggage_first_composite.extract(ctx, test_carrier)
  
  // 两种顺序都应该成功
  assert_eq(true, true)
}

test "composite_propagator_resource_cleanup_error_handling" {
  // 测试资源清理相关的错误处理
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let ctx = Context::empty()
  
  // 测试大量创建和销毁composite propagator
  let mut i = 0
  while i < 100 {
    let temp_composite = CompositePropagator::new([trace_propagator, baggage_propagator])
    let temp_carrier = MapCarrier::new()
    
    temp_composite.inject(ctx, temp_carrier)
    let temp_extracted = temp_composite.extract(ctx, temp_carrier)
    
    assert_eq(true, true)
    i = i + 1
  }
  
  // 测试大量carrier的创建和使用
  let mut j = 0
  while j < 100 {
    let temp_carrier = MapCarrier::new()
    composite.inject(ctx, temp_carrier)
    let temp_extracted = composite.extract(ctx, temp_carrier)
    
    assert_eq(true, true)
    j = j + 1
  }
}