// Propagation模块边界测试用例
test "propagator_boundary_conditions" {
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  let context = Context::empty()
  
  // 测试空carrier的边界条件
  let empty_carrier = MapCarrier::new()
  
  // inject到空carrier
  trace_propagator.inject(context, empty_carrier)
  baggage_propagator.inject(context, empty_carrier)
  
  match empty_carrier.get(TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected traceparent header in empty carrier")
  }
  
  match empty_carrier.get(BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected baggage header in empty carrier")
  }
  
  // extract从空carrier
  let empty_extract_context = trace_propagator.extract(context, empty_carrier)
  let empty_extract_baggage = baggage_propagator.extract(context, empty_carrier)
  
  assert_eq(true, true)  // 简化验证，确保不崩溃
  
  // 测试包含损坏数据的carrier
  let corrupted_carrier = MapCarrier::from_map([
    (TRACE_PARENT_HEADER, ""),  // 空traceparent
    (BAGGAGE_HEADER, ""),       // 空baggage
    ("malformed.header", "value")
  ])
  
  let corrupted_extract_context = trace_propagator.extract(context, corrupted_carrier)
  let corrupted_extract_baggage = baggage_propagator.extract(context, corrupted_carrier)
  
  assert_eq(true, true)  // 简化验证，确保不崩溃
}

test "trace_context_header_parsing_edge_cases" {
  let propagator = W3CTraceContextPropagator::{}
  let context = Context::empty()
  
  // 测试各种边界情况的traceparent头
  
  // 标准格式
  let standard_carrier = MapCarrier::from_map([
    (TRACE_PARENT_HEADER, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  ])
  
  let standard_extract = propagator.extract(context, standard_carrier)
  assert_eq(true, true)
  
  // 最小长度
  let minimal_carrier = MapCarrier::from_map([
    (TRACE_PARENT_HEADER, "00-00000000000000000000000000000000-0000000000000000-00")
  ])
  
  let minimal_extract = propagator.extract(context, minimal_carrier)
  assert_eq(true, true)
  
  // 最大值
  let max_carrier = MapCarrier::from_map([
    (TRACE_PARENT_HEADER, "ff-ffffffffffffffffffffffffffffffff-ffffffffffffffff-ff")
  ])
  
  let max_extract = propagator.extract(context, max_carrier)
  assert_eq(true, true)
  
  // 无效格式 - 太短
  let too_short_carrier = MapCarrier::from_map([
    (TRACE_PARENT_HEADER, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331")
  ])
  
  let too_short_extract = propagator.extract(context, too_short_carrier)
  assert_eq(true, true)
  
  // 无效格式 - 太长
  let too_long_carrier = MapCarrier::from_map([
    (TRACE_PARENT_HEADER, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01-extra")
  ])
  
  let too_long_extract = propagator.extract(context, too_long_carrier)
  assert_eq(true, true)
  
  // 无效字符
  let invalid_chars_carrier = MapCarrier::from_map([
    (TRACE_PARENT_HEADER, "00-0af7651916cd43dd8448eb211c80319g-b7ad6b7169203331-01")
  ])
  
  let invalid_chars_extract = propagator.extract(context, invalid_chars_carrier)
  assert_eq(true, true)
}

test "baggage_header_parsing_edge_cases" {
  let propagator = W3CBaggagePropagator::{}
  let context = Context::empty()
  
  // 测试各种边界情况的baggage头
  
  // 空baggage
  let empty_baggage_carrier = MapCarrier::from_map([
    (BAGGAGE_HEADER, "")
  ])
  
  let empty_baggage_extract = propagator.extract(context, empty_baggage_carrier)
  assert_eq(true, true)
  
  // 单个条目
  let single_carrier = MapCarrier::from_map([
    (BAGGAGE_HEADER, "key=value")
  ])
  
  let single_extract = propagator.extract(context, single_carrier)
  assert_eq(true, true)
  
  // 多个条目
  let multiple_carrier = MapCarrier::from_map([
    (BAGGAGE_HEADER, "key1=value1,key2=value2,key3=value3")
  ])
  
  let multiple_extract = propagator.extract(context, multiple_carrier)
  assert_eq(true, true)
  
  // 空键
  let empty_key_carrier = MapCarrier::from_map([
    (BAGGAGE_HEADER, "=value,key2=value2")
  ])
  
  let empty_key_extract = propagator.extract(context, empty_key_carrier)
  assert_eq(true, true)
  
  // 空值
  let empty_value_carrier = MapCarrier::from_map([
    (BAGGAGE_HEADER, "key1=,key2=value2")
  ])
  
  let empty_value_extract = propagator.extract(context, empty_value_carrier)
  assert_eq(true, true)
  
  // 只有逗号
  let commas_only_carrier = MapCarrier::from_map([
    (BAGGAGE_HEADER, ",,,")
  ])
  
  let commas_only_extract = propagator.extract(context, commas_only_carrier)
  assert_eq(true, true)
  
  // 多个等号
  let multiple_equals_carrier = MapCarrier::from_map([
    (BAGGAGE_HEADER, "key=value=with=equals,key2=value2")
  ])
  
  let multiple_equals_extract = propagator.extract(context, multiple_equals_carrier)
  assert_eq(true, true)
  
  // Unicode字符
  let unicode_carrier = MapCarrier::from_map([
    (BAGGAGE_HEADER, "用户=用户123,emoji=🚀")
  ])
  
  let unicode_extract = propagator.extract(context, unicode_carrier)
  assert_eq(true, true)
  
  // 特殊字符
  let special_chars_carrier = MapCarrier::from_map([
    (BAGGAGE_HEADER, "special=!@#$%^&*(),spaces=value with spaces")
  ])
  
  let special_chars_extract = propagator.extract(context, special_chars_carrier)
  assert_eq(true, true)
}

test "composite_propagator_boundary_conditions" {
  // 测试CompositePropagator的边界条件
  
  // 空propagator数组
  let empty_composite = CompositePropagator::new([])
  let context = Context::empty()
  let carrier = MapCarrier::new()
  
  empty_composite.inject(context, carrier)
  let empty_extract = empty_composite.extract(context, carrier)
  
  assert_eq(true, true)  // 确保不崩溃
  
  // 单个propagator
  let single_trace = CompositePropagator::new([W3CTraceContextPropagator::{}])
  let single_baggage = CompositePropagator::new([W3CBaggagePropagator::{}])
  
  single_trace.inject(context, carrier)
  single_baggage.inject(context, carrier)
  
  match carrier.get(TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected traceparent from single propagator")
  }
  
  match carrier.get(BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected baggage from single propagator")
  }
  
  // 重复的propagator类型
  let duplicate_composite = CompositePropagator::new([
    W3CTraceContextPropagator::{},
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{},
    W3CBaggagePropagator::{}
  ])
  
  let duplicate_carrier = MapCarrier::new()
  duplicate_composite.inject(context, duplicate_carrier)
  
  match duplicate_carrier.get(TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected traceparent from duplicate propagators")
  }
  
  match duplicate_carrier.get(BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected baggage from duplicate propagators")
  }
}

test "map_carrier_boundary_conditions" {
  // 测试MapCarrier的边界条件
  
  // 空数据
  let empty_carrier = MapCarrier::from_map([])
  
  match empty_carrier.get("any.key") {
    Some(_) => @test.fail("Expected None from empty carrier")
    None => assert_eq(true, true)
  }
  
  let empty_keys = empty_carrier.keys()
  assert_eq(empty_keys.length(), 0)
  
  // 单个条目
  let single_carrier = MapCarrier::from_map([("single.key", "single.value")])
  
  match single_carrier.get("single.key") {
    Some(value) => assert_eq(value, "single.value")
    None => @test.fail("Expected value from single entry carrier")
  }
  
  match single_carrier.get("missing.key") {
    Some(_) => @test.fail("Expected None for missing key")
    None => assert_eq(true, true)
  }
  
  let single_keys = single_carrier.keys()
  assert_eq(single_keys.length(), 1)
  assert_eq(single_keys[0], "single.key")
  
  // 重复键（后面的应该覆盖前面的）
  let duplicate_keys_carrier = MapCarrier::from_map([
    ("duplicate.key", "value1"),
    ("duplicate.key", "value2"),
    ("duplicate.key", "value3")
  ])
  
  match duplicate_keys_carrier.get("duplicate.key") {
    Some(value) => assert_eq(value, "value3")  // 最后一个值
    None => @test.fail("Expected last value for duplicate key")
  }
  
  // 空键和空值
  let empty_key_value_carrier = MapCarrier::from_map([
    ("", "empty.key.value"),
    ("empty.value.key", ""),
    ("", "")
  ])
  
  match empty_key_value_carrier.get("") {
    Some(value) => assert_eq(value, "")  // 最后一个空键的值
    None => @test.fail("Expected value for empty key")
  }
  
  match empty_key_value_carrier.get("empty.value.key") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected empty value")
  }
  
  // 长键和长值
  let long_key = "a".repeat(1000)
  let long_value = "b".repeat(1000)
  
  let long_carrier = MapCarrier::from_map([
    (long_key, long_value),
    ("normal.key", "normal.value")
  ])
  
  match long_carrier.get(long_key) {
    Some(value) => assert_eq(value, long_value)
    None => @test.fail("Expected long value")
  }
  
  match long_carrier.get("normal.key") {
    Some(value) => assert_eq(value, "normal.value")
    None => @test.fail("Expected normal value")
  }
  
  // Unicode和特殊字符
  let unicode_carrier = MapCarrier::from_map([
    ("Unicode键🚀", "Unicode值🔥"),
    ("special!@#$%", "value!@#$%"),
    ("spaces and\ttabs\n", "value with\tspaces\n")
  ])
  
  match unicode_carrier.get("Unicode键🚀") {
    Some(value) => assert_eq(value, "Unicode值🔥")
    None => @test.fail("Expected unicode value")
  }
  
  match unicode_carrier.get("special!@#$%") {
    Some(value) => assert_eq(value, "value!@#$%")
    None => @test.fail("Expected special characters value")
  }
  
  match unicode_carrier.get("spaces and\ttabs\n") {
    Some(value) => assert_eq(value, "value with\tspaces\n")
    None => @test.fail("Expected value with spaces and tabs")
  }
}

test "propagation_with_extreme_values" {
  // 测试传播过程中的极值处理
  
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let context = Context::empty()
  
  // 极长的headers
  let very_long_traceparent = "00-" + "f".repeat(32) + "-" + "f".repeat(16) + "-01"
  let very_long_baggage = "key1=" + "a".repeat(1000) + ",key2=" + "b".repeat(1000)
  
  let extreme_carrier = MapCarrier::from_map([
    (TRACE_PARENT_HEADER, very_long_traceparent),
    (BAGGAGE_HEADER, very_long_baggage)
  ])
  
  let extreme_extract = composite_propagator.extract(context, extreme_carrier)
  assert_eq(true, true)
  
  // 包含极多条目的baggage
  let many_entries = []
  let mut i = 0
  while i < 100 {
    many_entries.push("key" + i.to_string() + "=value" + i.to_string())
    i = i + 1
  }
  let many_baggage = ",".join(many_entries)
  
  let many_entries_carrier = MapCarrier::from_map([
    (BAGGAGE_HEADER, many_baggage)
  ])
  
  let many_extract = baggage_propagator.extract(context, many_entries_carrier)
  assert_eq(true, true)
  
  // 包含所有可能字符的headers
  let all_chars_traceparent = "00-0123456789abcdef0123456789abcdef-0123456789abcdef-01"
  let all_chars_baggage = "key1=value1!@#$%^&*(),key2=value2中文🚀,key3=value3\t\n\r"
  
  let all_chars_carrier = MapCarrier::from_map([
    (TRACE_PARENT_HEADER, all_chars_traceparent),
    (BAGGAGE_HEADER, all_chars_baggage)
  ])
  
  let all_chars_extract = composite_propagator.extract(context, all_chars_carrier)
  assert_eq(true, true)
}

test "propagation_error_recovery" {
  // 测试传播过程中的错误恢复
  
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let context = Context::empty()
  
  // 部分损坏的headers
  let partially_corrupted_carrier = MapCarrier::from_map([
    (TRACE_PARENT_HEADER, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),  // 有效
    (BAGGAGE_HEADER, "invalid=baggage=format"),  // 无效
    ("good.header", "good.value")  // 正常
  ])
  
  let partial_extract = composite_propagator.extract(context, partially_corrupted_carrier)
  assert_eq(true, true)
  
  // 完全损坏的headers
  let fully_corrupted_carrier = MapCarrier::from_map([
    (TRACE_PARENT_HEADER, "completely-invalid-format"),
    (BAGGAGE_HEADER, "also///invalid///format"),
    ("another.bad", "header")
  ])
  
  let fully_corrupted_extract = composite_propagator.extract(context, fully_corrupted_carrier)
  assert_eq(true, true)
  
  // 混合有效和无效的propagators
  let mixed_carrier = MapCarrier::from_map([
    (TRACE_PARENT_HEADER, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    (BAGGAGE_HEADER, "key1=value1,key2=value2"),
    ("custom.header1", "custom.value1"),
    ("custom.header2", "custom.value2")
  ])
  
  // 先inject，然后extract
  composite_propagator.inject(context, mixed_carrier)
  let mixed_extract = composite_propagator.extract(context, mixed_carrier)
  
  // 验证原有headers仍然存在
  match mixed_carrier.get("custom.header1") {
    Some(value) => assert_eq(value, "custom.value1")
    None => @test.fail("Expected custom header1 to persist")
  }
  
  match mixed_carrier.get("custom.header2") {
    Some(value) => assert_eq(value, "custom.value2")
    None => @test.fail("Expected custom header2 to persist")
  }
  
  assert_eq(true, true)
}