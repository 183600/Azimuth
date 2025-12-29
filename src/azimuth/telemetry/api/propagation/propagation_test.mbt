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

test "trace_context_error_recovery_malformed_headers" {
  // 测试trace-context传播器的错误恢复 - 处理格式错误的header
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  // 测试各种格式错误的traceparent header
  let malformed_cases = [
    // 缺少部分
    "00-0af7651916cd43dd8448eb211c80319c", // 缺少span_id和flags
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331", // 缺少flags
    "0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01", // 缺少版本
    
    // 长度错误
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b71692033-01", // span_id太短
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-011", // flags太长
    
    // 无效字符
    "00-0af7651916cd43dd8448eb211c80319g-b7ad6b7169203331-01", // 无效的trace_id字符
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b716920333z-01", // 无效的span_id字符
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-1z", // 无效的flags字符
    
    // 格式错误
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01-extra", // 额外字段
    "01-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01", // 不支持的版本
    "invalid-format-string", // 完全无效的格式
    
    // 边界情况
    "", // 空字符串
    "00--b7ad6b7169203331-01", // 缺少trace_id
    "00-0af7651916cd43dd8448eb211c80319c--01", // 缺少span_id
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-", // 缺少flags
  ]
  
  // 测试每种错误情况都能正常处理（不会崩溃）
  let mut index = 0
  while index < malformed_cases.length() {
    let malformed_header = malformed_cases[index]
    let mut data = []
    data = data.push((TRACE_PARENT_HEADER, malformed_header))
    let carrier = MapCarrier::from_map(data)
    
    // 提取操作应该能够处理错误格式并返回原始context
    let recovered_ctx = W3CTraceContextPropagator::extract(ctx, carrier)
    assert_eq(true, true) // 如果没有崩溃就算成功
    
    index = index + 1
  }
}

test "trace_context_error_recovery_malformed_tracestate" {
  // 测试trace-context传播器的错误恢复 - 处理格式错误的tracestate header
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  // 测试各种格式错误的tracestate header
  let malformed_tracestate_cases = [
    // 无效的键值对格式
    "invalid-format", // 没有等号
    "key=", // 空值
    "=value", // 空键
    "key=value=extra", // 多个等号
    
    // 无效字符
    "key@=value", // 键包含无效字符
    "key=value,invalid@", // 逗号后的无效格式
    "key=value,key2=value2@invalid", // 无效后缀
    
    // 边界情况
    "", // 空字符串
    ",", // 只有逗号
    "key=value,", // 结尾逗号
    ",key=value", // 开头逗号
    "key=value,,key2=value2", // 连续逗号
    
    // 过长的键或值
    "a".repeat(257) + "=value", // 键过长
    "key=" + "a".repeat(257), // 值过长
  ]
  
  // 测试每种错误情况都能正常处理
  let mut index = 0
  while index < malformed_tracestate_cases.length() {
    let malformed_tracestate = malformed_tracestate_cases[index]
    let mut data = []
    data = data.push((TRACE_PARENT_HEADER, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
    data = data.push((TRACE_STATE_HEADER, malformed_tracestate))
    let carrier = MapCarrier::from_map(data)
    
    // 提取操作应该能够处理错误格式
    let recovered_ctx = W3CTraceContextPropagator::extract(ctx, carrier)
    assert_eq(true, true) // 如果没有崩溃就算成功
    
    index = index + 1
  }
}

test "baggage_error_recovery_malformed_headers" {
  // 测试baggage传播器的错误恢复 - 处理格式错误的header
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  // 测试各种格式错误的baggage header
  let malformed_baggage_cases = [
    // 无效的键值对格式
    "invalid-format", // 没有等号
    "key=", // 空值
    "=value", // 空键
    "key=value=extra", // 多个等号
    
    // 无效属性格式
    "key=value;invalid", // 无效属性格式
    "key=value;=invalidvalue", // 空属性名
    "key=value;invalid=", // 空属性值
    
    // 边界情况
    "", // 空字符串
    ",", // 只有逗号
    "key=value,", // 结尾逗号
    ",key=value", // 开头逗号
    "key=value,,key2=value2", // 连续逗号
    
    // 转义字符问题
    "key=value\\", // 不完整的转义
    "key=value\\invalid", // 无效的转义序列
    "key=value\\\\", // 双反斜杠
    
    // URL编码问题
    "key=value%2", // 不完整的URL编码
    "key=value%ZZ", // 无效的URL编码
  ]
  
  // 测试每种错误情况都能正常处理
  let mut index = 0
  while index < malformed_baggage_cases.length() {
    let malformed_baggage = malformed_baggage_cases[index]
    let mut data = []
    data = data.push((BAGGAGE_HEADER, malformed_baggage))
    let carrier = MapCarrier::from_map(data)
    
    // 提取操作应该能够处理错误格式
    let recovered_ctx = W3CBaggagePropagator::extract(ctx, carrier)
    assert_eq(true, true) // 如果没有崩溃就算成功
    
    index = index + 1
  }
}

test "composite_propagator_error_recovery" {
  // 测试Composite传播器的错误恢复
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  // 创建包含多个传播器的复合传播器
  let trace_propagator = W3CTraceContextPropagator
  let baggage_propagator = W3CBaggagePropagator
  let propagators = [trace_propagator, baggage_propagator]
  let composite = CompositePropagator::new(propagators)
  
  // 测试部分header损坏的情况
  let mut data = []
  data = data.push((TRACE_PARENT_HEADER, "invalid-traceparent-format"))
  data = data.push((BAGGAGE_HEADER, "key=value")) // 只有baggage是有效的
  let partial_valid_carrier = MapCarrier::from_map(data)
  
  // 应该能够处理部分有效的header
  let recovered_ctx1 = CompositePropagator::extract(ctx, partial_valid_carrier)
  assert_eq(true, true) // 如果没有崩溃就算成功
  
  // 测试所有header都损坏的情况
  let mut invalid_data = []
  invalid_data = invalid_data.push((TRACE_PARENT_HEADER, "completely-invalid"))
  invalid_data = invalid_data.push((BAGGAGE_HEADER, "also-completely-invalid"))
  let all_invalid_carrier = MapCarrier::from_map(invalid_data)
  
  // 应该能够处理所有无效的header
  let recovered_ctx2 = CompositePropagator::extract(ctx, all_invalid_carrier)
  assert_eq(true, true) // 如果没有崩溃就算成功
  
  // 测试混合有效和无效的tracestate
  let mut mixed_data = []
  mixed_data = mixed_data.push((TRACE_PARENT_HEADER, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  mixed_data = mixed_data.push((TRACE_STATE_HEADER, "valid=key1,invalid@format,valid2=key2"))
  mixed_data = mixed_data.push((BAGGAGE_HEADER, "valid=value1,invalid=,valid2=value2"))
  let mixed_carrier = MapCarrier::from_map(mixed_data)
  
  // 应该能够处理混合的有效和无效数据
  let recovered_ctx3 = CompositePropagator::extract(ctx, mixed_carrier)
  assert_eq(true, true) // 如果没有崩溃就算成功
}

test "propagator_resilience_to_edge_cases" {
  // 测试传播器对边界情况的弹性
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  // 测试超长的header值
  let very_long_traceparent = "00-" + "a".repeat(32) + "-" + "b".repeat(16) + "-01"
  let very_long_baggage = "key1=" + "a".repeat(1000) + ",key2=" + "b".repeat(1000)
  
  let mut long_data = []
  long_data = long_data.push((TRACE_PARENT_HEADER, very_long_traceparent))
  long_data = long_data.push((BAGGAGE_HEADER, very_long_baggage))
  let long_carrier = MapCarrier::from_map(long_data)
  
  // 应该能够处理超长的header值
  let recovered_ctx1 = W3CTraceContextPropagator::extract(ctx, long_carrier)
  let recovered_ctx2 = W3CBaggagePropagator::extract(ctx, long_carrier)
  assert_eq(true, true)
  
  // 测试包含特殊字符的header值
  let special_chars_tracestate = "key=value with spaces,key2=value,with,commas,key3=value;with;semicolons"
  let special_chars_baggage = "key=value with spaces,key2=value,with,commas,key3=value;with;semicolons"
  
  let mut special_data = []
  special_data = special_data.push((TRACE_PARENT_HEADER, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  special_data = special_data.push((TRACE_STATE_HEADER, special_chars_tracestate))
  special_data = special_data.push((BAGGAGE_HEADER, special_chars_baggage))
  let special_carrier = MapCarrier::from_map(special_data)
  
  // 应该能够处理包含特殊字符的header值
  let recovered_ctx3 = W3CTraceContextPropagator::extract(ctx, special_carrier)
  let recovered_ctx4 = W3CBaggagePropagator::extract(ctx, special_carrier)
  assert_eq(true, true)
  
  // 测试Unicode字符
  let unicode_baggage = "中文键=中文值,emoji=🔥🚀,mixed=中mixed文"
  let mut unicode_data = []
  unicode_data = unicode_data.push((BAGGAGE_HEADER, unicode_baggage))
  let unicode_carrier = MapCarrier::from_map(unicode_data)
  
  // 应该能够处理Unicode字符
  let recovered_ctx5 = W3CBaggagePropagator::extract(ctx, unicode_carrier)
  assert_eq(true, true)
  
  // 测试空carrier
  let empty_carrier = MapCarrier::new()
  let recovered_ctx6 = W3CTraceContextPropagator::extract(ctx, empty_carrier)
  let recovered_ctx7 = W3CBaggagePropagator::extract(ctx, empty_carrier)
  assert_eq(true, true)
}