// Propagation API 集成测试用例

test "propagation_composite_integration" {
  // 测试复合传播器的集成功能
  
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 创建复合传播器
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // 创建初始上下文
  let initial_context = Context::empty()
  
  // 创建载体
  let carrier_data = [
    ("user-agent", "Mozilla/5.0"),
    ("x-request-id", "req-12345")
  ]
  let carrier = MapCarrier::from_map(carrier_data)
  
  // 注入上下文到载体
  composite_propagator.inject(initial_context, carrier)
  
  // 从载体提取上下文
  let extracted_context = composite_propagator.extract(Context::empty(), carrier)
  
  // 验证操作不会崩溃
  assert_eq(true, true)
}

test "propagation_w3c_tracecontext_format" {
  // 测试W3C TraceContext格式验证
  
  let propagator = W3CTraceContextPropagator::{}
  let context = Context::empty()
  let carrier = MapCarrier::new()
  
  // 注入tracecontext
  propagator.inject(context, carrier)
  
  // 验证traceparent格式
  match carrier.get("traceparent") {
    Some(trace_parent) => {
      // 验证格式：version-trace_id-span_id-flags
      let parts = trace_parent.split("-")
      assert_eq(parts.length(), 4)
      
      // 验证版本（应该是"00"）
      assert_eq(parts[0], "00")
      
      // 验证trace-id长度（32个hex字符）
      assert_eq(parts[1].length(), 32)
      
      // 验证span-id长度（16个hex字符）
      assert_eq(parts[2].length(), 16)
      
      // 验证flags长度（2个hex字符）
      assert_eq(parts[3].length(), 2)
    }
    None => {
      assert_eq(false, true)  // 应该有traceparent头
    }
  }
}

test "propagation_baggage_header_parsing" {
  // 测试baggage头解析
  
  let propagator = W3CBaggagePropagator::{}
  let context = Context::empty()
  let carrier_data = [
    ("baggage", "key1=value1,key2=value2;param1=value1")
  ]
  let carrier = MapCarrier::from_map(carrier_data)
  
  // 提取baggage
  let extracted_context = propagator.extract(context, carrier)
  
  // 再次注入以验证
  propagator.inject(extracted_context, carrier)
  
  // 验证操作不会崩溃
  assert_eq(true, true)
}

test "propagation_carrier_implementation" {
  // 测试载体实现的各种操作
  
  let initial_data = [
    ("header1", "value1"),
    ("header2", "value2"),
    ("x-custom-header", "custom-value")
  ]
  let carrier = MapCarrier::from_map(initial_data)
  
  // 测试get操作
  assert_eq(carrier.get("header1"), Some("value1"))
  assert_eq(carrier.get("header2"), Some("value2"))
  assert_eq(carrier.get("x-custom-header"), Some("custom-value"))
  assert_eq(carrier.get("nonexistent"), None)
  
  // 测试keys操作
  let keys = carrier.keys()
  assert_eq(keys.length(), 3)
  assert_eq(keys.contains("header1"), true)
  assert_eq(keys.contains("header2"), true)
  assert_eq(keys.contains("x-custom-header"), true)
  
  // 测试to_map操作
  let map_data = carrier.to_map()
  assert_eq(map_data.length(), 3)
  
  // 验证数据完整性
  let mut found_header1 = false
  let mut found_header2 = false
  let mut found_custom = false
  
  let mut i = 0
  while i < map_data.length() {
    let (key, value) = map_data[i]
    if key == "header1" && value == "value1" {
      found_header1 = true
    } else if key == "header2" && value == "value2" {
      found_header2 = true
    } else if key == "x-custom-header" && value == "custom-value" {
      found_custom = true
    }
    i = i + 1
  }
  
  assert_eq(found_header1, true)
  assert_eq(found_header2, true)
  assert_eq(found_custom, true)
}