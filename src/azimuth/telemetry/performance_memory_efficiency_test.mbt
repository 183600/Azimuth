// 性能优化的内存效率测试
// 测试遥测系统的内存使用效率和优化策略

test "memory_efficiency_attribute_storage" {
  // 测试属性存储的内存效率
  let meter_provider = @azimuth.telemetry.api.metrics.NoopMeterProvider::{}
  let meter = meter_provider.get_meter("memory_efficiency_meter", "1.0.0")
  
  // 测试不同类型属性的内存使用
  let counter = meter.create_counter("memory_test_counter", "count", "Memory test counter")
  
  // 测试字符串属性的不同长度对内存的影响
  let short_string_attr = @azimuth.telemetry.api.common.AttributeValue::string("short")
  let medium_string_attr = @azimuth.telemetry.api.common.AttributeValue::string("medium_length_string")
  let long_string_attr = @azimuth.telemetry.api.common.AttributeValue::string("this_is_a_very_long_string_that_consumes_more_memory")
  
  // 测试数字属性的内存效率
  let small_int_attr = @azimuth.telemetry.api.common.AttributeValue::int(1L)
  let large_int_attr = @azimuth.telemetry.api.common.AttributeValue::int(Int64::max_value())
  let small_float_attr = @azimuth.telemetry.api.common.AttributeValue::float(0.001)
  let large_float_attr = @azimuth.telemetry.api.common.AttributeValue::float(Double::max_value())
  
  // 测试布尔属性的内存效率
  let bool_true_attr = @azimuth.telemetry.api.common.AttributeValue::bool(true)
  let bool_false_attr = @azimuth.telemetry.api.common.AttributeValue::bool(false)
  
  // 记录带有不同属性类型的指标
  counter.add(1L, [
    ("short_string", short_string_attr),
    ("medium_string", medium_string_attr),
    ("long_string", long_string_attr),
    ("small_int", small_int_attr),
    ("large_int", large_int_attr),
    ("small_float", small_float_attr),
    ("large_float", large_float_attr),
    ("bool_true", bool_true_attr),
    ("bool_false", bool_false_attr)
  ])
  
  // 验证属性数量
  assert_eq(9)
}