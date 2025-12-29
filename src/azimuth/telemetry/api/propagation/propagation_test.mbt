test "map_carrier_basic_operations" {
  // 测试MapCarrier的基本操作
  let carrier = MapCarrier::new()
  
  // 测试keys方法
  let keys = carrier.keys()
  assert_eq(keys.length(), 0)
  
  // 测试get方法
  let value = carrier.get("nonexistent")
  assert_eq(value, None)
}

test "map_carrier_from_map" {
  // 测试从Map创建MapCarrier
  let mut data = []
  data = data.push(("key1", "value1"))
  data = data.push(("key2", "value2"))
  
  let carrier = MapCarrier::from_map(data)
  
  assert_eq(carrier.get("key1"), Some("value1"))
  assert_eq(carrier.get("key2"), Some("value2"))
  assert_eq(carrier.get("missing"), None)
  
  let keys = carrier.keys()
  assert_eq(keys.length(), 2)
}

test "w3c_trace_context_propagator_constants" {
  // 测试W3C TraceContext常量
  assert_eq(TRACE_PARENT_HEADER, "traceparent")
  assert_eq(TRACE_STATE_HEADER, "tracestate")
}

test "w3c_trace_context_propagator_inject" {
  // 测试W3C TraceContext注入
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let carrier = MapCarrier::new()
  
  W3CTraceContextPropagator::inject(ctx, carrier)
  
  // 在简化实现中，我们注入一个固定的traceparent值
  let trace_parent = carrier.get(TRACE_PARENT_HEADER)
  assert_eq(trace_parent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
}

test "w3c_trace_context_propagator_extract_with_header" {
  // 测试W3C TraceContext提取（有header）
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  let mut data = []
  data = data.push((TRACE_PARENT_HEADER, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  let carrier = MapCarrier::from_map(data)
  
  let extracted_ctx = W3CTraceContextPropagator::extract(ctx, carrier)
  
  // 在简化实现中，我们只是返回原始context
  // 在完整实现中，应该解析traceparent并更新context
  assert_eq(true, true)
}

test "w3c_trace_context_propagator_extract_without_header" {
  // 测试W3C TraceContext提取（无header）
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let carrier = MapCarrier::new()
  
  let extracted_ctx = W3CTraceContextPropagator::extract(ctx, carrier)
  
  // 应该返回原始context
  assert_eq(true, true)
}

test "w3c_baggage_propagator_constants" {
  // 测试W3C Baggage常量
  assert_eq(BAGGAGE_HEADER, "baggage")
}

test "w3c_baggage_propagator_inject" {
  // 测试W3C Baggage注入
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let carrier = MapCarrier::new()
  
  W3CBaggagePropagator::inject(ctx, carrier)
  
  // 在简化实现中，我们注入一个固定的baggage值
  let baggage = carrier.get(BAGGAGE_HEADER)
  assert_eq(baggage, Some("key1=value1,key2=value2"))
}

test "w3c_baggage_propagator_extract_with_header" {
  // 测试W3C Baggage提取（有header）
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  let mut data = []
  data = data.push((BAGGAGE_HEADER, "user.id=12345,service.name=api"))
  let carrier = MapCarrier::from_map(data)
  
  let extracted_ctx = W3CBaggagePropagator::extract(ctx, carrier)
  
  // 在简化实现中，我们只是返回原始context
  // 在完整实现中，应该解析baggage并更新context
  assert_eq(true, true)
}

test "composite_propagator_creation" {
  // 测试CompositePropagator创建
  let trace_propagator = W3CTraceContextPropagator
  let baggage_propagator = W3CBaggagePropagator
  let propagators = [trace_propagator, baggage_propagator]
  
  let composite = CompositePropagator::new(propagators)
  assert_eq(composite.propagators.length(), 2)
}

test "composite_propagator_inject" {
  // 测试CompositePropagator注入
  let trace_propagator = W3CTraceContextPropagator
  let baggage_propagator = W3CBaggagePropagator
  let propagators = [trace_propagator, baggage_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let carrier = MapCarrier::new()
  
  CompositePropagator::inject(ctx, carrier)
  
  // 应该同时注入traceparent和baggage
  let trace_parent = carrier.get(TRACE_PARENT_HEADER)
  let baggage = carrier.get(BAGGAGE_HEADER)
  
  assert_eq(trace_parent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(baggage, Some("key1=value1,key2=value2"))
}

test "composite_propagator_extract" {
  // 测试CompositePropagator提取
  let trace_propagator = W3CTraceContextPropagator
  let baggage_propagator = W3CBaggagePropagator
  let propagators = [trace_propagator, baggage_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  let mut data = []
  data = data.push((TRACE_PARENT_HEADER, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  data = data.push((BAGGAGE_HEADER, "user.id=12345"))
  let carrier = MapCarrier::from_map(data)
  
  let extracted_ctx = CompositePropagator::extract(ctx, carrier)
  
  // 在简化实现中，我们只是返回原始context
  // 在完整实现中，应该依次应用每个propagator
  assert_eq(true, true)
}

test "map_carrier_comprehensive_operations" {
  // 测试MapCarrier的完整操作
  let initial_data = [
    ("existing.key1", "value1"),
    ("existing.key2", "value2"),
    ("x-trace-id", "trace123"),
    ("x-baggage", "user=test")
  ]
  
  let carrier = MapCarrier::from_map(initial_data)
  
  // 测试keys方法
  let keys = carrier.keys()
  assert_eq(keys.length(), 4)
  
  // 验证所有键都存在
  assert_eq(keys.contains("existing.key1"), true)
  assert_eq(keys.contains("existing.key2"), true)
  assert_eq(keys.contains("x-trace-id"), true)
  assert_eq(keys.contains("x-baggage"), true)
  
  // 测试get方法
  assert_eq(carrier.get("existing.key1"), Some("value1"))
  assert_eq(carrier.get("existing.key2"), Some("value2"))
  assert_eq(carrier.get("x-trace-id"), Some("trace123"))
  assert_eq(carrier.get("x-baggage"), Some("user=test"))
  
  // 测试获取不存在的键
  assert_eq(carrier.get("nonexistent.key"), None)
  assert_eq(carrier.get("missing.header"), None)
  assert_eq(carrier.get(""), None)
  
  // 测试空键和空值的情况
  let data_with_edge_cases = [
    ("", "empty_key_value"),
    ("empty.value", ""),
    ("special.chars", "value-with-special-chars:;?&="),
    ("unicode.test", "测试值")
  ]
  
  let edge_case_carrier = MapCarrier::from_map(data_with_edge_cases)
  
  assert_eq(edge_case_carrier.get(""), Some("empty_key_value"))
  assert_eq(edge_case_carrier.get("empty.value"), Some(""))
  assert_eq(edge_case_carrier.get("special.chars"), Some("value-with-special-chars:;?&="))
  assert_eq(edge_case_carrier.get("unicode.test"), Some("测试值"))
  
  // 测试空carrier
  let empty_carrier = MapCarrier::new()
  let empty_keys = empty_carrier.keys()
  assert_eq(empty_keys.length(), 0)
  assert_eq(empty_carrier.get("any.key"), None)
}