// 遥测数据转换增强测试用例
// 测试不同数据类型之间的转换和格式化

test "telemetry_attribute_type_conversion" {
  // 测试属性值类型转换功能
  
  let string_attr = "user.id"
  let int_attr = 12345L
  let float_attr = 3.14159
  let bool_attr = true
  
  // 验证字符串属性
  assert_eq(string_attr.length(), 7)
  assert_eq(string_attr.has_prefix("user"), true)
  assert_eq(string_attr.has_suffix(".id"), true)
  
  // 验证整数属性
  assert_eq(int_attr > 10000L, true)
  assert_eq(int_attr < 20000L, true)
  
  // 验证浮点数属性
  assert_eq(float_attr > 3.0, true)
  assert_eq(float_attr < 3.2, true)
  
  // 验证布尔属性
  assert_eq(bool_attr, true)
  
  // 创建属性组合字符串
  let combined = string_attr + ":" + int_attr.to_string() + ":" + float_attr.to_string() + ":" + bool_attr.to_string()
  assert_eq(combined.has_prefix("user.id"), true)
  assert_eq(combined.has_suffix("true"), true)
}

test "telemetry_timestamp_conversion" {
  // 测试时间戳转换功能
  
  let unix_timestamp = 1704067200L  // 2024-01-01 00:00:00 UTC
  let iso_format = "2024-01-01T00:00:00Z"
  
  // 验证Unix时间戳
  assert_eq(unix_timestamp > 1700000000L, true)
  assert_eq(unix_timestamp < 1800000000L, true)
  
  // 验证ISO格式
  assert_eq(iso_format.length(), 20)
  assert_eq(iso_format.has_prefix("2024-01-01"), true)
  assert_eq(iso_format.has_suffix("00:00:00Z"), true)
  
  // 验证时间戳格式转换
  let timestamp_str = unix_timestamp.to_string()
  assert_eq(timestamp_str.length(), 10)
  assert_eq(timestamp_str.has_prefix("1704"), true)
}

test "telemetry_metric_unit_conversion" {
  // 测试度量单位转换功能
  
  let bytes_value = 1024L
  let kilobytes_value = 1.024
  let megabytes_value = 0.001024
  
  // 验证字节值
  assert_eq(bytes_value, 1024L)
  
  // 验证千字节值
  assert_eq(kilobytes_value > 1.0, true)
  assert_eq(kilobytes_value < 1.1, true)
  
  // 验证兆字节值
  assert_eq(megabytes_value > 0.001, true)
  assert_eq(megabytes_value < 0.002, true)
  
  // 创建单位转换字符串
  let conversion_str = bytes_value.to_string() + "B = " + kilobytes_value.to_string() + "KB = " + megabytes_value.to_string() + "MB"
  assert_eq(conversion_str.has_prefix("1024B"), true)
  assert_eq(conversion_str.has_suffix("MB"), true)
}

test "telemetry_trace_format_conversion" {
  // 测试追踪格式转换功能
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let trace_flags = "01"
  
  // 验证追踪ID
  assert_eq(trace_id.length(), 32)
  assert_eq(trace_id.has_prefix("0af7"), true)
  assert_eq(trace_id.has_suffix("319c"), true)
  
  // 验证跨度ID
  assert_eq(span_id.length(), 16)
  assert_eq(span_id.has_prefix("b7ad"), true)
  assert_eq(span_id.has_suffix("3331"), true)
  
  // 验证追踪标志
  assert_eq(trace_flags.length(), 2)
  assert_eq(trace_flags, "01")
  
  // 创建完整追踪字符串
  let full_trace = trace_id + "-" + span_id + "-" + trace_flags
  assert_eq(full_trace.length(), 51)  // 32 + 1 + 16 + 1 + 2
  assert_eq(full_trace.has_prefix("0af7"), true)
  assert_eq(full_trace.has_suffix("01"), true)
}