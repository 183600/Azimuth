// 指标API的边界条件和异常处理测试
// 测试各种边界条件和异常情况下的指标API行为

test "metrics_counter_boundary_conditions" {
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test_meter", "1.0.0")
  
  // 测试Counter的边界条件
  let counter = meter.create_counter("test_counter", "count", "Test counter")
  
  // 测试零值
  counter.add(0L, [])
  
  // 测试正数边界
  counter.add(Int64::max_value(), [])
  
  // 测试负数（应该被允许）
  counter.add(-1L, [])
  
  // 测试最小负数
  counter.add(Int64::min_value(), [])
  
  // 测试空属性数组
  counter.add(1L, [])
  
  // 测试大量属性
  let large_attributes = Array::range_with(0, 1000, 1).map(fn(i) { ("attr_" + i.to_string(), AttributeValue::int(i.to_int64())) })
  counter.add(1L, large_attributes)
}

test "metrics_histogram_boundary_conditions" {
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test_meter", "1.0.0")
  
  // 测试Histogram的边界条件
  let histogram = meter.create_histogram("test_histogram", "ms", "Test histogram")
  
  // 测试零值
  histogram.record(0.0, [])
  
  // 测试正数边界
  histogram.record(Double::max_value(), [])
  
  // 测试负数
  histogram.record(-1.0, [])
  
  // 测试最小负数
  histogram.record(Double::min_value(), [])
  
  // 测试无穷大
  histogram.record(Double::infinity, [])
  
  // 测试负无穷大
  histogram.record(Double::neg_infinity, [])
  
  // 测试NaN
  histogram.record(Double::nan, [])
  
  // 测试非常小的值
  histogram.record(Double::min_positive, [])
}

test "metrics_up_down_counter_boundary_conditions" {
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test_meter", "1.0.0")
  
  // 测试UpDownCounter的边界条件
  let up_down_counter = meter.create_up_down_counter("test_up_down_counter", "count", "Test up down counter")
  
  // 测试零值
  up_down_counter.add(0L, [])
  
  // 测试正数边界
  up_down_counter.add(Int64::max_value(), [])
  
  // 测试负数边界
  up_down_counter.add(Int64::min_value(), [])
  
  // 测试增加和减少的组合
  up_down_counter.add(100L, [])
  up_down_counter.add(-50L, [])
  up_down_counter.add(-150L, [])
}

test "metrics_gauge_boundary_conditions" {
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test_meter", "1.0.0")
  
  // 测试Gauge的边界条件
  let gauge = meter.create_gauge("test_gauge", "value", "Test gauge")
  
  // 测试零值
  gauge.record(0.0, [])
  
  // 测试正数边界
  gauge.record(Double::max_value(), [])
  
  // 测试负数边界
  gauge.record(Double::min_value(), [])
  
  // 测试无穷大
  gauge.record(Double::infinity, [])
  
  // 测试负无穷大
  gauge.record(Double::neg_infinity, [])
  
  // 测试NaN
  gauge.record(Double::nan, [])
  
  // 测试科学计数法
  gauge.record(1.23e-10, [])
  gauge.record(1.23e20, [])
}

test "metrics_instrument_name_validation" {
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test_meter", "1.0.0")
  
  // 测试空名称
  let empty_name_counter = meter.create_counter("", "count", "Empty name counter")
  empty_name_counter.add(1L, [])
  
  // 测试非常长的名称
  let long_name = "a" * 1000
  let long_name_counter = meter.create_counter(long_name, "count", "Long name counter")
  long_name_counter.add(1L, [])
  
  // 测试特殊字符名称
  let special_chars_counter = meter.create_counter("test-counter_123.test", "count", "Special chars counter")
  special_chars_counter.add(1L, [])
  
  // 测试Unicode名称
  let unicode_counter = meter.create_counter("测试计数器", "count", "Unicode counter")
  unicode_counter.add(1L, [])
}

test "metrics_attribute_boundary_conditions" {
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test_meter", "1.0.0")
  
  let counter = meter.create_counter("test_counter", "count", "Test counter")
  
  // 测试空属性键
  counter.add(1L, [("", AttributeValue::string("value"))])
  
  // 测试空属性值
  counter.add(1L, [("key", AttributeValue::string(""))])
  
  // 测试非常长的属性键
  let long_key = "a" * 1000
  counter.add(1L, [(long_key, AttributeValue::string("value"))])
  
  // 测试非常长的属性值
  let long_value = "a" * 1000
  counter.add(1L, [("key", AttributeValue::string(long_value))])
  
  // 测试特殊字符属性
  counter.add(1L, [("special-chars_123", AttributeValue::string("special!@#$%^&*()"))])
  
  // 测试Unicode属性
  counter.add(1L, [("unicode键", AttributeValue::string("Unicode值"))])
  
  // 测试所有属性类型
  counter.add(1L, [
    ("string_attr", AttributeValue::string("string_value")),
    ("int_attr", AttributeValue::int(42L)),
    ("float_attr", AttributeValue::float(3.14)),
    ("bool_attr", AttributeValue::bool(true)),
    ("array_string_attr", AttributeValue::array_string(["a", "b", "c"])),
    ("array_int_attr", AttributeValue::array_int([1L, 2L, 3L])),
    ("array_float_attr", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("array_bool_attr", AttributeValue::array_bool([true, false, true]))
  ])
}

test "metrics_meter_provider_edge_cases" {
  // 测试空名称的meter
  let meter_provider1 = metrics::NoopMeterProvider::{}
  let meter1 = meter_provider1.get_meter("", "1.0.0", "")
  
  // 测试非常长的meter名称
  let long_meter_name = "a" * 1000
  let meter2 = meter_provider1.get_meter(long_meter_name, "1.0.0", "")
  
  // 测试特殊字符meter名称
  let meter3 = meter_provider1.get_meter("test-meter_123.test", "1.0.0", "")
  
  // 测试Unicode meter名称
  let meter4 = meter_provider1.get_meter("测试遥测", "1.0.0", "")
  
  // 测试空版本
  let meter5 = meter_provider1.get_meter("test_meter", "", "")
  
  // 测试非常长的版本
  let long_version = "1.0.0-" + ("a" * 1000)
  let meter6 = meter_provider1.get_meter("test_meter", long_version, "")
  
  // 测试空schema URL
  let meter7 = meter_provider1.get_meter("test_meter", "1.0.0", "")
  
  // 测试非常长的schema URL
  let long_schema_url = "https://example.com/" + ("a" * 1000)
  let meter8 = meter_provider1.get_meter("test_meter", "1.0.0", long_schema_url)
  
  // 确保所有meter都能正常工作
  let counter1 = meter1.create_counter("counter1", "count", "Counter 1")
  let counter2 = meter2.create_counter("counter2", "count", "Counter 2")
  let counter3 = meter3.create_counter("counter3", "count", "Counter 3")
  let counter4 = meter4.create_counter("counter4", "count", "Counter 4")
  let counter5 = meter5.create_counter("counter5", "count", "Counter 5")
  let counter6 = meter6.create_counter("counter6", "count", "Counter 6")
  let counter7 = meter7.create_counter("counter7", "count", "Counter 7")
  let counter8 = meter8.create_counter("counter8", "count", "Counter 8")
  
  counter1.add(1L, [])
  counter2.add(1L, [])
  counter3.add(1L, [])
  counter4.add(1L, [])
  counter5.add(1L, [])
  counter6.add(1L, [])
  counter7.add(1L, [])
  counter8.add(1L, [])
}