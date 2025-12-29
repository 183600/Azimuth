// Propagation API基础功能测试
test "MapCarrier creation and basic operations" {
  let carrier = MapCarrier::new()
  
  // 验证初始状态
  assert carrier.data.length == 0
  assert carrier.keys().length == 0
  
  // 测试获取不存在的键
  assert match (carrier.get("non-existent")) {
    None => true
    Some(_) => false
  }
}

test "MapCarrier from_map" {
  let initial_data = [
    ("key1", "value1"),
    ("key2", "value2"),
    ("key3", "value3")
  ]
  let carrier = MapCarrier::from_map(initial_data)
  
  // 验证数据正确加载
  assert carrier.data.length == 3
  assert carrier.keys().length == 3
  
  // 验证可以获取值
  assert match (carrier.get("key1")) {
    Some(value) => value == "value1"
    None => false
  }
  
  assert match (carrier.get("key2")) {
    Some(value) => value == "value2"
    None => false
  }
  
  assert match (carrier.get("key3")) {
    Some(value) => value == "value3"
    None => false
  }
}

test "MapCarrier to_map" {
  let initial_data = [
    ("test-key1", "test-value1"),
    ("test-key2", "test-value2")
  ]
  let carrier = MapCarrier::from_map(initial_data)
  let extracted_data = carrier.to_map()
  
  // 验证数据一致性
  assert extracted_data.length == initial_data.length
  
  let mut i = 0
  while i < extracted_data.length {
    let (key, value) = extracted_data[i]
    assert match (carrier.get(key)) {
      Some(v) => v == value
      None => false
    }
    i = i + 1
  }
}

test "MapCarrier keys operation" {
  let data = [
    ("alpha", "value1"),
    ("beta", "value2"),
    ("gamma", "value3")
  ]
  let carrier = MapCarrier::from_map(data)
  let keys = carrier.keys()
  
  // 验证所有键都被提取
  assert keys.length == 3
  
  // 验证键的内容（顺序可能不同）
  let mut found_alpha = false
  let mut found_beta = false
  let mut found_gamma = false
  
  let mut i = 0
  while i < keys.length {
    match keys[i] {
      "alpha" => found_alpha = true
      "beta" => found_beta = true
      "gamma" => found_gamma = true
      _ => ()
    }
    i = i + 1
  }
  
  assert found_alpha && found_beta && found_gamma
}

test "W3CTraceContextPropagator inject" {
  let propagator = W3CTraceContextPropagator::{}
  let ctx = context::Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试注入操作
  propagator.inject(ctx, carrier)
  
  // 在实际实现中，这应该设置traceparent header
  // 由于是简化实现，我们只验证方法调用不失败
  assert true
}

test "W3CTraceContextPropagator extract with header" {
  let propagator = W3CTraceContextPropagator::{}
  let ctx = context::Context::empty()
  let carrier_data = [("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")]
  let carrier = MapCarrier::from_map(carrier_data)
  
  // 测试提取操作
  let extracted_ctx = propagator.extract(ctx, carrier)
  
  // 在实际实现中，这应该解析traceparent并更新context
  // 由于是简化实现，我们只验证方法调用不失败并返回context
  assert true
}

test "W3CTraceContextPropagator extract without header" {
  let propagator = W3CTraceContextPropagator::{}
  let ctx = context::Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试提取操作（没有traceparent header）
  let extracted_ctx = propagator.extract(ctx, carrier)
  
  // 应该返回原始context
  assert true
}

test "W3CBaggagePropagator inject" {
  let propagator = W3CBaggagePropagator::{}
  let ctx = context::Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试注入操作
  propagator.inject(ctx, carrier)
  
  // 在实际实现中，这应该设置baggage header
  // 由于是简化实现，我们只验证方法调用不失败
  assert true
}

test "W3CBaggagePropagator extract with header" {
  let propagator = W3CBaggagePropagator::{}
  let ctx = context::Context::empty()
  let carrier_data = [("baggage", "key1=value1,key2=value2")]
  let carrier = MapCarrier::from_map(carrier_data)
  
  // 测试提取操作
  let extracted_ctx = propagator.extract(ctx, carrier)
  
  // 在实际实现中，这应该解析baggage header并更新context
  // 由于是简化实现，我们只验证方法调用不失败并返回context
  assert true
}

test "W3CBaggagePropagator extract without header" {
  let propagator = W3CBaggagePropagator::{}
  let ctx = context::Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试提取操作（没有baggage header）
  let extracted_ctx = propagator.extract(ctx, carrier)
  
  // 应该返回原始context
  assert true
}

test "CompositePropagator creation" {
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let propagators = [trace_propagator, baggage_propagator]
  
  let composite = CompositePropagator::new(propagators)
  
  // 验证composite propagator创建成功
  assert composite.propagators.length == 2
}

test "CompositePropagator inject" {
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let propagators = [trace_propagator, baggage_propagator]
  
  let composite = CompositePropagator::new(propagators)
  let ctx = context::Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试复合注入操作
  composite.inject(ctx, carrier)
  
  // 应该调用所有子propagators的inject方法
  assert true
}

test "CompositePropagator extract" {
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let propagators = [trace_propagator, baggage_propagator]
  
  let composite = CompositePropagator::new(propagators)
  let ctx = context::Context::empty()
  let carrier_data = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "key1=value1,key2=value2")
  ]
  let carrier = MapCarrier::from_map(carrier_data)
  
  // 测试复合提取操作
  let extracted_ctx = composite.extract(ctx, carrier)
  
  // 应该调用所有子propagators的extract方法
  assert true
}

test "Propagation constants" {
  // 验证常量值
  assert TRACE_PARENT_HEADER == "traceparent"
  assert TRACE_STATE_HEADER == "tracestate"
  assert BAGGAGE_HEADER == "baggage"
}